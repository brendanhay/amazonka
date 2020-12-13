{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AccessControlTranslation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AccessControlTranslation
  ( AccessControlTranslation (..),

    -- * Smart constructor
    mkAccessControlTranslation,

    -- * Lenses
    actOwner,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.OwnerOverride

-- | A container for information about access control for replicas.
--
-- /See:/ 'mkAccessControlTranslation' smart constructor.
newtype AccessControlTranslation = AccessControlTranslation'
  { -- | Specifies the replica ownership. For default and valid values, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT bucket replication> in the /Amazon Simple Storage Service API Reference/ .
    owner :: OwnerOverride
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccessControlTranslation' with the minimum fields required to make a request.
--
-- * 'owner' - Specifies the replica ownership. For default and valid values, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT bucket replication> in the /Amazon Simple Storage Service API Reference/ .
mkAccessControlTranslation ::
  -- | 'owner'
  OwnerOverride ->
  AccessControlTranslation
mkAccessControlTranslation pOwner_ =
  AccessControlTranslation' {owner = pOwner_}

-- | Specifies the replica ownership. For default and valid values, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT bucket replication> in the /Amazon Simple Storage Service API Reference/ .
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actOwner :: Lens.Lens' AccessControlTranslation OwnerOverride
actOwner = Lens.lens (owner :: AccessControlTranslation -> OwnerOverride) (\s a -> s {owner = a} :: AccessControlTranslation)
{-# DEPRECATED actOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

instance Lude.FromXML AccessControlTranslation where
  parseXML x = AccessControlTranslation' Lude.<$> (x Lude..@ "Owner")

instance Lude.ToXML AccessControlTranslation where
  toXML AccessControlTranslation' {..} =
    Lude.mconcat ["Owner" Lude.@= owner]
