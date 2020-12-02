{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.UsagePlanKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.UsagePlanKey where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a usage plan key to identify a plan customer.
--
--
-- To associate an API stage with a selected API key in a usage plan, you must create a UsagePlanKey resource to represent the selected 'ApiKey' .
--
-- " <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans>
--
-- /See:/ 'usagePlanKey' smart constructor.
data UsagePlanKey = UsagePlanKey'
  { _upkValue :: !(Maybe Text),
    _upkName :: !(Maybe Text),
    _upkId :: !(Maybe Text),
    _upkType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UsagePlanKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upkValue' - The value of a usage plan key.
--
-- * 'upkName' - The name of a usage plan key.
--
-- * 'upkId' - The Id of a usage plan key.
--
-- * 'upkType' - The type of a usage plan key. Currently, the valid key type is @API_KEY@ .
usagePlanKey ::
  UsagePlanKey
usagePlanKey =
  UsagePlanKey'
    { _upkValue = Nothing,
      _upkName = Nothing,
      _upkId = Nothing,
      _upkType = Nothing
    }

-- | The value of a usage plan key.
upkValue :: Lens' UsagePlanKey (Maybe Text)
upkValue = lens _upkValue (\s a -> s {_upkValue = a})

-- | The name of a usage plan key.
upkName :: Lens' UsagePlanKey (Maybe Text)
upkName = lens _upkName (\s a -> s {_upkName = a})

-- | The Id of a usage plan key.
upkId :: Lens' UsagePlanKey (Maybe Text)
upkId = lens _upkId (\s a -> s {_upkId = a})

-- | The type of a usage plan key. Currently, the valid key type is @API_KEY@ .
upkType :: Lens' UsagePlanKey (Maybe Text)
upkType = lens _upkType (\s a -> s {_upkType = a})

instance FromJSON UsagePlanKey where
  parseJSON =
    withObject
      "UsagePlanKey"
      ( \x ->
          UsagePlanKey'
            <$> (x .:? "value")
            <*> (x .:? "name")
            <*> (x .:? "id")
            <*> (x .:? "type")
      )

instance Hashable UsagePlanKey

instance NFData UsagePlanKey
