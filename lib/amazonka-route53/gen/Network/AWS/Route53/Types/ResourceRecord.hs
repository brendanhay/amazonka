{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ResourceRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ResourceRecord where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

-- | Information specific to the resource record.
--
--
--
-- /See:/ 'resourceRecord' smart constructor.
newtype ResourceRecord = ResourceRecord' {_rrValue :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrValue' - The current or new DNS record value, not to exceed 4,000 characters. In the case of a @DELETE@ action, if the current value does not match the actual value, an error is returned. For descriptions about how to format @Value@ for different record types, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/ResourceRecordTypes.html Supported DNS Resource Record Types> in the /Amazon Route 53 Developer Guide/ . You can specify more than one value for all record types except @CNAME@ and @SOA@ .
resourceRecord ::
  -- | 'rrValue'
  Text ->
  ResourceRecord
resourceRecord pValue_ = ResourceRecord' {_rrValue = pValue_}

-- | The current or new DNS record value, not to exceed 4,000 characters. In the case of a @DELETE@ action, if the current value does not match the actual value, an error is returned. For descriptions about how to format @Value@ for different record types, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/ResourceRecordTypes.html Supported DNS Resource Record Types> in the /Amazon Route 53 Developer Guide/ . You can specify more than one value for all record types except @CNAME@ and @SOA@ .
rrValue :: Lens' ResourceRecord Text
rrValue = lens _rrValue (\s a -> s {_rrValue = a})

instance FromXML ResourceRecord where
  parseXML x = ResourceRecord' <$> (x .@ "Value")

instance Hashable ResourceRecord

instance NFData ResourceRecord

instance ToXML ResourceRecord where
  toXML ResourceRecord' {..} = mconcat ["Value" @= _rrValue]
