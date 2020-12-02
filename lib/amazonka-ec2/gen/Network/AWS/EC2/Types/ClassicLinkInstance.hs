{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClassicLinkInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClassicLinkInstance where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a linked EC2-Classic instance.
--
--
--
-- /See:/ 'classicLinkInstance' smart constructor.
data ClassicLinkInstance = ClassicLinkInstance'
  { _cliInstanceId ::
      !(Maybe Text),
    _cliGroups :: !(Maybe [GroupIdentifier]),
    _cliVPCId :: !(Maybe Text),
    _cliTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClassicLinkInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cliInstanceId' - The ID of the instance.
--
-- * 'cliGroups' - A list of security groups.
--
-- * 'cliVPCId' - The ID of the VPC.
--
-- * 'cliTags' - Any tags assigned to the instance.
classicLinkInstance ::
  ClassicLinkInstance
classicLinkInstance =
  ClassicLinkInstance'
    { _cliInstanceId = Nothing,
      _cliGroups = Nothing,
      _cliVPCId = Nothing,
      _cliTags = Nothing
    }

-- | The ID of the instance.
cliInstanceId :: Lens' ClassicLinkInstance (Maybe Text)
cliInstanceId = lens _cliInstanceId (\s a -> s {_cliInstanceId = a})

-- | A list of security groups.
cliGroups :: Lens' ClassicLinkInstance [GroupIdentifier]
cliGroups = lens _cliGroups (\s a -> s {_cliGroups = a}) . _Default . _Coerce

-- | The ID of the VPC.
cliVPCId :: Lens' ClassicLinkInstance (Maybe Text)
cliVPCId = lens _cliVPCId (\s a -> s {_cliVPCId = a})

-- | Any tags assigned to the instance.
cliTags :: Lens' ClassicLinkInstance [Tag]
cliTags = lens _cliTags (\s a -> s {_cliTags = a}) . _Default . _Coerce

instance FromXML ClassicLinkInstance where
  parseXML x =
    ClassicLinkInstance'
      <$> (x .@? "instanceId")
      <*> (x .@? "groupSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "vpcId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable ClassicLinkInstance

instance NFData ClassicLinkInstance
