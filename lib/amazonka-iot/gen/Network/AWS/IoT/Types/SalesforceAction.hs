{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SalesforceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SalesforceAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an action to write a message to a Salesforce IoT Cloud Input Stream.
--
--
--
-- /See:/ 'salesforceAction' smart constructor.
data SalesforceAction = SalesforceAction'
  { _saToken :: !Text,
    _saUrl :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SalesforceAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saToken' - The token used to authenticate access to the Salesforce IoT Cloud Input Stream. The token is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
--
-- * 'saUrl' - The URL exposed by the Salesforce IoT Cloud Input Stream. The URL is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
salesforceAction ::
  -- | 'saToken'
  Text ->
  -- | 'saUrl'
  Text ->
  SalesforceAction
salesforceAction pToken_ pUrl_ =
  SalesforceAction' {_saToken = pToken_, _saUrl = pUrl_}

-- | The token used to authenticate access to the Salesforce IoT Cloud Input Stream. The token is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
saToken :: Lens' SalesforceAction Text
saToken = lens _saToken (\s a -> s {_saToken = a})

-- | The URL exposed by the Salesforce IoT Cloud Input Stream. The URL is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
saUrl :: Lens' SalesforceAction Text
saUrl = lens _saUrl (\s a -> s {_saUrl = a})

instance FromJSON SalesforceAction where
  parseJSON =
    withObject
      "SalesforceAction"
      (\x -> SalesforceAction' <$> (x .: "token") <*> (x .: "url"))

instance Hashable SalesforceAction

instance NFData SalesforceAction

instance ToJSON SalesforceAction where
  toJSON SalesforceAction' {..} =
    object
      (catMaybes [Just ("token" .= _saToken), Just ("url" .= _saUrl)])
