{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.ComplianceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.ComplianceDetails
  ( ComplianceDetails (..),

    -- * Smart constructor
    mkComplianceDetails,

    -- * Lenses
    cdKeysWithNoncompliantValues,
    cdComplianceStatus,
    cdNoncompliantKeys,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information that shows whether a resource is compliant with the effective tag policy, including details on any noncompliant tag keys.
--
-- /See:/ 'mkComplianceDetails' smart constructor.
data ComplianceDetails = ComplianceDetails'
  { keysWithNoncompliantValues ::
      Lude.Maybe [Lude.Text],
    complianceStatus :: Lude.Maybe Lude.Bool,
    noncompliantKeys :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComplianceDetails' with the minimum fields required to make a request.
--
-- * 'complianceStatus' - Whether a resource is compliant with the effective tag policy.
-- * 'keysWithNoncompliantValues' - These are keys defined in the effective policy that are on the resource with either incorrect case treatment or noncompliant values.
-- * 'noncompliantKeys' - These tag keys on the resource are noncompliant with the effective tag policy.
mkComplianceDetails ::
  ComplianceDetails
mkComplianceDetails =
  ComplianceDetails'
    { keysWithNoncompliantValues = Lude.Nothing,
      complianceStatus = Lude.Nothing,
      noncompliantKeys = Lude.Nothing
    }

-- | These are keys defined in the effective policy that are on the resource with either incorrect case treatment or noncompliant values.
--
-- /Note:/ Consider using 'keysWithNoncompliantValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdKeysWithNoncompliantValues :: Lens.Lens' ComplianceDetails (Lude.Maybe [Lude.Text])
cdKeysWithNoncompliantValues = Lens.lens (keysWithNoncompliantValues :: ComplianceDetails -> Lude.Maybe [Lude.Text]) (\s a -> s {keysWithNoncompliantValues = a} :: ComplianceDetails)
{-# DEPRECATED cdKeysWithNoncompliantValues "Use generic-lens or generic-optics with 'keysWithNoncompliantValues' instead." #-}

-- | Whether a resource is compliant with the effective tag policy.
--
-- /Note:/ Consider using 'complianceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdComplianceStatus :: Lens.Lens' ComplianceDetails (Lude.Maybe Lude.Bool)
cdComplianceStatus = Lens.lens (complianceStatus :: ComplianceDetails -> Lude.Maybe Lude.Bool) (\s a -> s {complianceStatus = a} :: ComplianceDetails)
{-# DEPRECATED cdComplianceStatus "Use generic-lens or generic-optics with 'complianceStatus' instead." #-}

-- | These tag keys on the resource are noncompliant with the effective tag policy.
--
-- /Note:/ Consider using 'noncompliantKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdNoncompliantKeys :: Lens.Lens' ComplianceDetails (Lude.Maybe [Lude.Text])
cdNoncompliantKeys = Lens.lens (noncompliantKeys :: ComplianceDetails -> Lude.Maybe [Lude.Text]) (\s a -> s {noncompliantKeys = a} :: ComplianceDetails)
{-# DEPRECATED cdNoncompliantKeys "Use generic-lens or generic-optics with 'noncompliantKeys' instead." #-}

instance Lude.FromJSON ComplianceDetails where
  parseJSON =
    Lude.withObject
      "ComplianceDetails"
      ( \x ->
          ComplianceDetails'
            Lude.<$> (x Lude..:? "KeysWithNoncompliantValues" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ComplianceStatus")
            Lude.<*> (x Lude..:? "NoncompliantKeys" Lude..!= Lude.mempty)
      )
