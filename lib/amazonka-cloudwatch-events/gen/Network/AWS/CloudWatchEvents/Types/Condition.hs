{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.Condition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.Condition where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A JSON string which you can use to limit the event bus permissions you are granting to only accounts that fulfill the condition. Currently, the only supported condition is membership in a certain AWS organization. The string must contain @Type@ , @Key@ , and @Value@ fields. The @Value@ field specifies the ID of the AWS organization. Following is an example value for @Condition@ :
--
--
-- @'{"Type" : "StringEquals", "Key": "aws:PrincipalOrgID", "Value": "o-1234567890"}'@
--
--
-- /See:/ 'condition' smart constructor.
data Condition = Condition'
  { _cType :: !Text,
    _cKey :: !Text,
    _cValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cType' - Specifies the type of condition. Currently the only supported value is @StringEquals@ .
--
-- * 'cKey' - Specifies the key for the condition. Currently the only supported key is @aws:PrincipalOrgID@ .
--
-- * 'cValue' - Specifies the value for the key. Currently, this must be the ID of the organization.
condition ::
  -- | 'cType'
  Text ->
  -- | 'cKey'
  Text ->
  -- | 'cValue'
  Text ->
  Condition
condition pType_ pKey_ pValue_ =
  Condition' {_cType = pType_, _cKey = pKey_, _cValue = pValue_}

-- | Specifies the type of condition. Currently the only supported value is @StringEquals@ .
cType :: Lens' Condition Text
cType = lens _cType (\s a -> s {_cType = a})

-- | Specifies the key for the condition. Currently the only supported key is @aws:PrincipalOrgID@ .
cKey :: Lens' Condition Text
cKey = lens _cKey (\s a -> s {_cKey = a})

-- | Specifies the value for the key. Currently, this must be the ID of the organization.
cValue :: Lens' Condition Text
cValue = lens _cValue (\s a -> s {_cValue = a})

instance Hashable Condition

instance NFData Condition

instance ToJSON Condition where
  toJSON Condition' {..} =
    object
      ( catMaybes
          [ Just ("Type" .= _cType),
            Just ("Key" .= _cKey),
            Just ("Value" .= _cValue)
          ]
      )
