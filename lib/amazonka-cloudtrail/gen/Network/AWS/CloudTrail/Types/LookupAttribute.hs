{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.LookupAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.LookupAttribute where

import Network.AWS.CloudTrail.Types.LookupAttributeKey
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies an attribute and value that filter the events returned.
--
--
--
-- /See:/ 'lookupAttribute' smart constructor.
data LookupAttribute = LookupAttribute'
  { _laAttributeKey ::
      !LookupAttributeKey,
    _laAttributeValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LookupAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laAttributeKey' - Specifies an attribute on which to filter the events returned.
--
-- * 'laAttributeValue' - Specifies a value for the specified AttributeKey.
lookupAttribute ::
  -- | 'laAttributeKey'
  LookupAttributeKey ->
  -- | 'laAttributeValue'
  Text ->
  LookupAttribute
lookupAttribute pAttributeKey_ pAttributeValue_ =
  LookupAttribute'
    { _laAttributeKey = pAttributeKey_,
      _laAttributeValue = pAttributeValue_
    }

-- | Specifies an attribute on which to filter the events returned.
laAttributeKey :: Lens' LookupAttribute LookupAttributeKey
laAttributeKey = lens _laAttributeKey (\s a -> s {_laAttributeKey = a})

-- | Specifies a value for the specified AttributeKey.
laAttributeValue :: Lens' LookupAttribute Text
laAttributeValue = lens _laAttributeValue (\s a -> s {_laAttributeValue = a})

instance Hashable LookupAttribute

instance NFData LookupAttribute

instance ToJSON LookupAttribute where
  toJSON LookupAttribute' {..} =
    object
      ( catMaybes
          [ Just ("AttributeKey" .= _laAttributeKey),
            Just ("AttributeValue" .= _laAttributeValue)
          ]
      )
