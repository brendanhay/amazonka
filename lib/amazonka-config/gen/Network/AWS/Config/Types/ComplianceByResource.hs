-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ComplianceByResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceByResource
  ( ComplianceByResource (..),

    -- * Smart constructor
    mkComplianceByResource,

    -- * Lenses
    cbrResourceId,
    cbrResourceType,
    cbrCompliance,
  )
where

import Network.AWS.Config.Types.Compliance
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether an AWS resource that is evaluated according to one or more AWS Config rules is compliant. A resource is compliant if it complies with all of the rules that evaluate it. A resource is noncompliant if it does not comply with one or more of these rules.
--
-- /See:/ 'mkComplianceByResource' smart constructor.
data ComplianceByResource = ComplianceByResource'
  { resourceId ::
      Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe Lude.Text,
    compliance :: Lude.Maybe Compliance
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComplianceByResource' with the minimum fields required to make a request.
--
-- * 'compliance' - Indicates whether the AWS resource complies with all of the AWS Config rules that evaluated it.
-- * 'resourceId' - The ID of the AWS resource that was evaluated.
-- * 'resourceType' - The type of the AWS resource that was evaluated.
mkComplianceByResource ::
  ComplianceByResource
mkComplianceByResource =
  ComplianceByResource'
    { resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      compliance = Lude.Nothing
    }

-- | The ID of the AWS resource that was evaluated.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrResourceId :: Lens.Lens' ComplianceByResource (Lude.Maybe Lude.Text)
cbrResourceId = Lens.lens (resourceId :: ComplianceByResource -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: ComplianceByResource)
{-# DEPRECATED cbrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the AWS resource that was evaluated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrResourceType :: Lens.Lens' ComplianceByResource (Lude.Maybe Lude.Text)
cbrResourceType = Lens.lens (resourceType :: ComplianceByResource -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: ComplianceByResource)
{-# DEPRECATED cbrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Indicates whether the AWS resource complies with all of the AWS Config rules that evaluated it.
--
-- /Note:/ Consider using 'compliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrCompliance :: Lens.Lens' ComplianceByResource (Lude.Maybe Compliance)
cbrCompliance = Lens.lens (compliance :: ComplianceByResource -> Lude.Maybe Compliance) (\s a -> s {compliance = a} :: ComplianceByResource)
{-# DEPRECATED cbrCompliance "Use generic-lens or generic-optics with 'compliance' instead." #-}

instance Lude.FromJSON ComplianceByResource where
  parseJSON =
    Lude.withObject
      "ComplianceByResource"
      ( \x ->
          ComplianceByResource'
            Lude.<$> (x Lude..:? "ResourceId")
            Lude.<*> (x Lude..:? "ResourceType")
            Lude.<*> (x Lude..:? "Compliance")
      )
