{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.DelegationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.DelegationSet where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

-- | A complex type that lists the name servers in a delegation set, as well as the @CallerReference@ and the @ID@ for the delegation set.
--
--
--
-- /See:/ 'delegationSet' smart constructor.
data DelegationSet = DelegationSet'
  { _dsId :: !(Maybe ResourceId),
    _dsCallerReference :: !(Maybe Text),
    _dsNameServers :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DelegationSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsId' - The ID that Amazon Route 53 assigns to a reusable delegation set.
--
-- * 'dsCallerReference' - The value that you specified for @CallerReference@ when you created the reusable delegation set.
--
-- * 'dsNameServers' - A complex type that contains a list of the authoritative name servers for a hosted zone or for a reusable delegation set.
delegationSet ::
  -- | 'dsNameServers'
  NonEmpty Text ->
  DelegationSet
delegationSet pNameServers_ =
  DelegationSet'
    { _dsId = Nothing,
      _dsCallerReference = Nothing,
      _dsNameServers = _List1 # pNameServers_
    }

-- | The ID that Amazon Route 53 assigns to a reusable delegation set.
dsId :: Lens' DelegationSet (Maybe ResourceId)
dsId = lens _dsId (\s a -> s {_dsId = a})

-- | The value that you specified for @CallerReference@ when you created the reusable delegation set.
dsCallerReference :: Lens' DelegationSet (Maybe Text)
dsCallerReference = lens _dsCallerReference (\s a -> s {_dsCallerReference = a})

-- | A complex type that contains a list of the authoritative name servers for a hosted zone or for a reusable delegation set.
dsNameServers :: Lens' DelegationSet (NonEmpty Text)
dsNameServers = lens _dsNameServers (\s a -> s {_dsNameServers = a}) . _List1

instance FromXML DelegationSet where
  parseXML x =
    DelegationSet'
      <$> (x .@? "Id")
      <*> (x .@? "CallerReference")
      <*> (x .@? "NameServers" .!@ mempty >>= parseXMLList1 "NameServer")

instance Hashable DelegationSet

instance NFData DelegationSet
