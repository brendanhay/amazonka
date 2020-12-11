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

import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configured access rules for the domain's document and search endpoints, and the current status of those rules.
--
-- /See:/ 'mkAccessPoliciesStatus' smart constructor.
data AccessPoliciesStatus = AccessPoliciesStatus'
  { options ::
      Lude.Text,
    status :: OptionStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccessPoliciesStatus' with the minimum fields required to make a request.
--
-- * 'options' - The access policy configured for the Elasticsearch domain. Access policies may be resource-based, IP-based, or IAM-based. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-access-policies Configuring Access Policies> for more information.
-- * 'status' - The status of the access policy for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
mkAccessPoliciesStatus ::
  -- | 'options'
  Lude.Text ->
  -- | 'status'
  OptionStatus ->
  AccessPoliciesStatus
mkAccessPoliciesStatus pOptions_ pStatus_ =
  AccessPoliciesStatus' {options = pOptions_, status = pStatus_}

-- | The access policy configured for the Elasticsearch domain. Access policies may be resource-based, IP-based, or IAM-based. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-access-policies Configuring Access Policies> for more information.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsOptions :: Lens.Lens' AccessPoliciesStatus Lude.Text
apsOptions = Lens.lens (options :: AccessPoliciesStatus -> Lude.Text) (\s a -> s {options = a} :: AccessPoliciesStatus)
{-# DEPRECATED apsOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The status of the access policy for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsStatus :: Lens.Lens' AccessPoliciesStatus OptionStatus
apsStatus = Lens.lens (status :: AccessPoliciesStatus -> OptionStatus) (\s a -> s {status = a} :: AccessPoliciesStatus)
{-# DEPRECATED apsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromJSON AccessPoliciesStatus where
  parseJSON =
    Lude.withObject
      "AccessPoliciesStatus"
      ( \x ->
          AccessPoliciesStatus'
            Lude.<$> (x Lude..: "Options") Lude.<*> (x Lude..: "Status")
      )
