{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthInfo where

import Network.AWS.IoT.Types.ActionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A collection of authorization information.
--
--
--
-- /See:/ 'authInfo' smart constructor.
data AuthInfo = AuthInfo'
  { _aiActionType :: !(Maybe ActionType),
    _aiResources :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuthInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiActionType' - The type of action for which the principal is being authorized.
--
-- * 'aiResources' - The resources for which the principal is being authorized to perform the specified action.
authInfo ::
  AuthInfo
authInfo =
  AuthInfo' {_aiActionType = Nothing, _aiResources = mempty}

-- | The type of action for which the principal is being authorized.
aiActionType :: Lens' AuthInfo (Maybe ActionType)
aiActionType = lens _aiActionType (\s a -> s {_aiActionType = a})

-- | The resources for which the principal is being authorized to perform the specified action.
aiResources :: Lens' AuthInfo [Text]
aiResources = lens _aiResources (\s a -> s {_aiResources = a}) . _Coerce

instance FromJSON AuthInfo where
  parseJSON =
    withObject
      "AuthInfo"
      ( \x ->
          AuthInfo'
            <$> (x .:? "actionType") <*> (x .:? "resources" .!= mempty)
      )

instance Hashable AuthInfo

instance NFData AuthInfo

instance ToJSON AuthInfo where
  toJSON AuthInfo' {..} =
    object
      ( catMaybes
          [ ("actionType" .=) <$> _aiActionType,
            Just ("resources" .= _aiResources)
          ]
      )
