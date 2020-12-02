{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.CognitoOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.CognitoOptionsStatus where

import Network.AWS.ElasticSearch.Types.CognitoOptions
import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Status of the Cognito options for the specified Elasticsearch domain.
--
--
--
-- /See:/ 'cognitoOptionsStatus' smart constructor.
data CognitoOptionsStatus = CognitoOptionsStatus'
  { _cosOptions ::
      !CognitoOptions,
    _cosStatus :: !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CognitoOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cosOptions' - Specifies the Cognito options for the specified Elasticsearch domain.
--
-- * 'cosStatus' - Specifies the status of the Cognito options for the specified Elasticsearch domain.
cognitoOptionsStatus ::
  -- | 'cosOptions'
  CognitoOptions ->
  -- | 'cosStatus'
  OptionStatus ->
  CognitoOptionsStatus
cognitoOptionsStatus pOptions_ pStatus_ =
  CognitoOptionsStatus'
    { _cosOptions = pOptions_,
      _cosStatus = pStatus_
    }

-- | Specifies the Cognito options for the specified Elasticsearch domain.
cosOptions :: Lens' CognitoOptionsStatus CognitoOptions
cosOptions = lens _cosOptions (\s a -> s {_cosOptions = a})

-- | Specifies the status of the Cognito options for the specified Elasticsearch domain.
cosStatus :: Lens' CognitoOptionsStatus OptionStatus
cosStatus = lens _cosStatus (\s a -> s {_cosStatus = a})

instance FromJSON CognitoOptionsStatus where
  parseJSON =
    withObject
      "CognitoOptionsStatus"
      ( \x ->
          CognitoOptionsStatus' <$> (x .: "Options") <*> (x .: "Status")
      )

instance Hashable CognitoOptionsStatus

instance NFData CognitoOptionsStatus
