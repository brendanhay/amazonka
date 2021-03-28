{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ReplicationConfiguration
  ( ReplicationConfiguration (..)
  -- * Smart constructor
  , mkReplicationConfiguration
  -- * Lenses
  , rcRole
  , rcRules
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.ReplicationRule as Types
import qualified Network.AWS.S3.Types.Role as Types

-- | A container for replication rules. You can add up to 1,000 rules. The maximum size of a replication configuration is 2 MB.
--
-- /See:/ 'mkReplicationConfiguration' smart constructor.
data ReplicationConfiguration = ReplicationConfiguration'
  { role' :: Types.Role
    -- ^ The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that Amazon S3 assumes when replicating objects. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-how-setup.html How to Set Up Replication> in the /Amazon Simple Storage Service Developer Guide/ .
  , rules :: [Types.ReplicationRule]
    -- ^ A container for one or more replication rules. A replication configuration must have at least one rule and can contain a maximum of 1,000 rules. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicationConfiguration' value with any optional fields omitted.
mkReplicationConfiguration
    :: Types.Role -- ^ 'role\''
    -> ReplicationConfiguration
mkReplicationConfiguration role'
  = ReplicationConfiguration'{role', rules = Core.mempty}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that Amazon S3 assumes when replicating objects. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-how-setup.html How to Set Up Replication> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRole :: Lens.Lens' ReplicationConfiguration Types.Role
rcRole = Lens.field @"role'"
{-# INLINEABLE rcRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

-- | A container for one or more replication rules. A replication configuration must have at least one rule and can contain a maximum of 1,000 rules. 
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRules :: Lens.Lens' ReplicationConfiguration [Types.ReplicationRule]
rcRules = Lens.field @"rules"
{-# INLINEABLE rcRules #-}
{-# DEPRECATED rules "Use generic-lens or generic-optics with 'rules' instead"  #-}

instance Core.ToXML ReplicationConfiguration where
        toXML ReplicationConfiguration{..}
          = Core.toXMLElement "Role" role' Core.<>
              Core.toXMLList "Rule" rules

instance Core.FromXML ReplicationConfiguration where
        parseXML x
          = ReplicationConfiguration' Core.<$>
              (x Core..@ "Role") Core.<*> x Core..@ "Rule" Core..@! Core.mempty
