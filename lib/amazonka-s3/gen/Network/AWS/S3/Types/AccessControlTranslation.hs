{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AccessControlTranslation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.AccessControlTranslation
  ( AccessControlTranslation (..)
  -- * Smart constructor
  , mkAccessControlTranslation
  -- * Lenses
  , actOwner
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.OwnerOverride as Types

-- | A container for information about access control for replicas.
--
-- /See:/ 'mkAccessControlTranslation' smart constructor.
newtype AccessControlTranslation = AccessControlTranslation'
  { owner :: Types.OwnerOverride
    -- ^ Specifies the replica ownership. For default and valid values, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT bucket replication> in the /Amazon Simple Storage Service API Reference/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AccessControlTranslation' value with any optional fields omitted.
mkAccessControlTranslation
    :: Types.OwnerOverride -- ^ 'owner'
    -> AccessControlTranslation
mkAccessControlTranslation owner = AccessControlTranslation'{owner}

-- | Specifies the replica ownership. For default and valid values, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT bucket replication> in the /Amazon Simple Storage Service API Reference/ .
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actOwner :: Lens.Lens' AccessControlTranslation Types.OwnerOverride
actOwner = Lens.field @"owner"
{-# INLINEABLE actOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

instance Core.ToXML AccessControlTranslation where
        toXML AccessControlTranslation{..}
          = Core.toXMLElement "Owner" owner

instance Core.FromXML AccessControlTranslation where
        parseXML x = AccessControlTranslation' Core.<$> (x Core..@ "Owner")
