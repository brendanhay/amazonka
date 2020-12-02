{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.IPRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.IPRange where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | Describes an IP range used in a security group.
--
--
--
-- /See:/ 'ipRange' smart constructor.
data IPRange = IPRange'
  { _irStatus :: !(Maybe Text),
    _irCIdRIP :: !(Maybe Text),
    _irTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IPRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irStatus' - The status of the IP range, for example, "authorized".
--
-- * 'irCIdRIP' - The IP range in Classless Inter-Domain Routing (CIDR) notation.
--
-- * 'irTags' - The list of tags for the IP range.
ipRange ::
  IPRange
ipRange =
  IPRange'
    { _irStatus = Nothing,
      _irCIdRIP = Nothing,
      _irTags = Nothing
    }

-- | The status of the IP range, for example, "authorized".
irStatus :: Lens' IPRange (Maybe Text)
irStatus = lens _irStatus (\s a -> s {_irStatus = a})

-- | The IP range in Classless Inter-Domain Routing (CIDR) notation.
irCIdRIP :: Lens' IPRange (Maybe Text)
irCIdRIP = lens _irCIdRIP (\s a -> s {_irCIdRIP = a})

-- | The list of tags for the IP range.
irTags :: Lens' IPRange [Tag]
irTags = lens _irTags (\s a -> s {_irTags = a}) . _Default . _Coerce

instance FromXML IPRange where
  parseXML x =
    IPRange'
      <$> (x .@? "Status")
      <*> (x .@? "CIDRIP")
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable IPRange

instance NFData IPRange
