{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KGKeyPairIds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KGKeyPairIds where

import Network.AWS.CloudFront.Types.KeyPairIds
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of identifiers for the public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
--
--
-- /See:/ 'kGKeyPairIds' smart constructor.
data KGKeyPairIds = KGKeyPairIds'
  { _kgkpiKeyPairIds ::
      !(Maybe KeyPairIds),
    _kgkpiKeyGroupId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KGKeyPairIds' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kgkpiKeyPairIds' - Undocumented member.
--
-- * 'kgkpiKeyGroupId' - The identifier of the key group that contains the public keys.
kGKeyPairIds ::
  KGKeyPairIds
kGKeyPairIds =
  KGKeyPairIds'
    { _kgkpiKeyPairIds = Nothing,
      _kgkpiKeyGroupId = Nothing
    }

-- | Undocumented member.
kgkpiKeyPairIds :: Lens' KGKeyPairIds (Maybe KeyPairIds)
kgkpiKeyPairIds = lens _kgkpiKeyPairIds (\s a -> s {_kgkpiKeyPairIds = a})

-- | The identifier of the key group that contains the public keys.
kgkpiKeyGroupId :: Lens' KGKeyPairIds (Maybe Text)
kgkpiKeyGroupId = lens _kgkpiKeyGroupId (\s a -> s {_kgkpiKeyGroupId = a})

instance FromXML KGKeyPairIds where
  parseXML x =
    KGKeyPairIds' <$> (x .@? "KeyPairIds") <*> (x .@? "KeyGroupId")

instance Hashable KGKeyPairIds

instance NFData KGKeyPairIds
