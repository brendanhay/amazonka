{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExceptionResourceKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExceptionResourceKey where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details that identify a resource within AWS Config, including the resource type and resource ID.
--
--
--
-- /See:/ 'remediationExceptionResourceKey' smart constructor.
data RemediationExceptionResourceKey = RemediationExceptionResourceKey'
  { _rerkResourceId ::
      !(Maybe Text),
    _rerkResourceType ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemediationExceptionResourceKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rerkResourceId' - The ID of the resource (for example., sg-xxxxxx).
--
-- * 'rerkResourceType' - The type of a resource.
remediationExceptionResourceKey ::
  RemediationExceptionResourceKey
remediationExceptionResourceKey =
  RemediationExceptionResourceKey'
    { _rerkResourceId = Nothing,
      _rerkResourceType = Nothing
    }

-- | The ID of the resource (for example., sg-xxxxxx).
rerkResourceId :: Lens' RemediationExceptionResourceKey (Maybe Text)
rerkResourceId = lens _rerkResourceId (\s a -> s {_rerkResourceId = a})

-- | The type of a resource.
rerkResourceType :: Lens' RemediationExceptionResourceKey (Maybe Text)
rerkResourceType = lens _rerkResourceType (\s a -> s {_rerkResourceType = a})

instance FromJSON RemediationExceptionResourceKey where
  parseJSON =
    withObject
      "RemediationExceptionResourceKey"
      ( \x ->
          RemediationExceptionResourceKey'
            <$> (x .:? "ResourceId") <*> (x .:? "ResourceType")
      )

instance Hashable RemediationExceptionResourceKey

instance NFData RemediationExceptionResourceKey

instance ToJSON RemediationExceptionResourceKey where
  toJSON RemediationExceptionResourceKey' {..} =
    object
      ( catMaybes
          [ ("ResourceId" .=) <$> _rerkResourceId,
            ("ResourceType" .=) <$> _rerkResourceType
          ]
      )
