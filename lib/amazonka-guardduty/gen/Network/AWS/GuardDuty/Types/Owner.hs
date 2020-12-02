{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Owner
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Owner where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information on the owner of the bucket.
--
--
--
-- /See:/ 'owner' smart constructor.
newtype Owner = Owner' {_oId :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Owner' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oId' - The canonical user ID of the bucket owner. For information about locating your canonical user ID see <https://docs.aws.amazon.com/general/latest/gr/acct-identifiers.html#FindingCanonicalId Finding Your Account Canonical User ID.>
owner ::
  Owner
owner = Owner' {_oId = Nothing}

-- | The canonical user ID of the bucket owner. For information about locating your canonical user ID see <https://docs.aws.amazon.com/general/latest/gr/acct-identifiers.html#FindingCanonicalId Finding Your Account Canonical User ID.>
oId :: Lens' Owner (Maybe Text)
oId = lens _oId (\s a -> s {_oId = a})

instance FromJSON Owner where
  parseJSON = withObject "Owner" (\x -> Owner' <$> (x .:? "id"))

instance Hashable Owner

instance NFData Owner
