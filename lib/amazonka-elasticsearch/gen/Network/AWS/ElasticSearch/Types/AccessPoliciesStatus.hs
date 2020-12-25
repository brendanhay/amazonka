{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.AccessPoliciesStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AccessPoliciesStatus
  ( AccessPoliciesStatus (..),

    -- * Smart constructor
    mkAccessPoliciesStatus,

    -- * Lenses
    apsOptions,
    apsStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types.OptionStatus as Types
import qualified Network.AWS.ElasticSearch.Types.PolicyDocument as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configured access rules for the domain's document and search endpoints, and the current status of those rules.
--
-- /See:/ 'mkAccessPoliciesStatus' smart constructor.
data AccessPoliciesStatus = AccessPoliciesStatus'
  { -- | The access policy configured for the Elasticsearch domain. Access policies may be resource-based, IP-based, or IAM-based. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-access-policies Configuring Access Policies> for more information.
    options :: Types.PolicyDocument,
    -- | The status of the access policy for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
    status :: Types.OptionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AccessPoliciesStatus' value with any optional fields omitted.
mkAccessPoliciesStatus ::
  -- | 'options'
  Types.PolicyDocument ->
  -- | 'status'
  Types.OptionStatus ->
  AccessPoliciesStatus
mkAccessPoliciesStatus options status =
  AccessPoliciesStatus' {options, status}

-- | The access policy configured for the Elasticsearch domain. Access policies may be resource-based, IP-based, or IAM-based. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-access-policies Configuring Access Policies> for more information.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsOptions :: Lens.Lens' AccessPoliciesStatus Types.PolicyDocument
apsOptions = Lens.field @"options"
{-# DEPRECATED apsOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The status of the access policy for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsStatus :: Lens.Lens' AccessPoliciesStatus Types.OptionStatus
apsStatus = Lens.field @"status"
{-# DEPRECATED apsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON AccessPoliciesStatus where
  parseJSON =
    Core.withObject "AccessPoliciesStatus" Core.$
      \x ->
        AccessPoliciesStatus'
          Core.<$> (x Core..: "Options") Core.<*> (x Core..: "Status")
