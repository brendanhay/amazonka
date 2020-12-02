{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ResourceRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ResourceRecord where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the domain name system (DNS) records to add to your domain's DNS to validate it for an Amazon Lightsail certificate.
--
--
--
-- /See:/ 'resourceRecord' smart constructor.
data ResourceRecord = ResourceRecord'
  { _rrValue :: !(Maybe Text),
    _rrName :: !(Maybe Text),
    _rrType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrValue' - The value for the DNS record.
--
-- * 'rrName' - The name of the record.
--
-- * 'rrType' - The DNS record type.
resourceRecord ::
  ResourceRecord
resourceRecord =
  ResourceRecord'
    { _rrValue = Nothing,
      _rrName = Nothing,
      _rrType = Nothing
    }

-- | The value for the DNS record.
rrValue :: Lens' ResourceRecord (Maybe Text)
rrValue = lens _rrValue (\s a -> s {_rrValue = a})

-- | The name of the record.
rrName :: Lens' ResourceRecord (Maybe Text)
rrName = lens _rrName (\s a -> s {_rrName = a})

-- | The DNS record type.
rrType :: Lens' ResourceRecord (Maybe Text)
rrType = lens _rrType (\s a -> s {_rrType = a})

instance FromJSON ResourceRecord where
  parseJSON =
    withObject
      "ResourceRecord"
      ( \x ->
          ResourceRecord'
            <$> (x .:? "value") <*> (x .:? "name") <*> (x .:? "type")
      )

instance Hashable ResourceRecord

instance NFData ResourceRecord
