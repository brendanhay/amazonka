{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PolicyVersion
  ( PolicyVersion (..),

    -- * Smart constructor
    mkPolicyVersion,

    -- * Lenses
    pvVersionId,
    pvCreateDate,
    pvIsDefaultVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a policy version.
--
-- /See:/ 'mkPolicyVersion' smart constructor.
data PolicyVersion = PolicyVersion'
  { -- | The policy version ID.
    versionId :: Lude.Maybe Lude.Text,
    -- | The date and time the policy was created.
    createDate :: Lude.Maybe Lude.Timestamp,
    -- | Specifies whether the policy version is the default.
    isDefaultVersion :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyVersion' with the minimum fields required to make a request.
--
-- * 'versionId' - The policy version ID.
-- * 'createDate' - The date and time the policy was created.
-- * 'isDefaultVersion' - Specifies whether the policy version is the default.
mkPolicyVersion ::
  PolicyVersion
mkPolicyVersion =
  PolicyVersion'
    { versionId = Lude.Nothing,
      createDate = Lude.Nothing,
      isDefaultVersion = Lude.Nothing
    }

-- | The policy version ID.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvVersionId :: Lens.Lens' PolicyVersion (Lude.Maybe Lude.Text)
pvVersionId = Lens.lens (versionId :: PolicyVersion -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: PolicyVersion)
{-# DEPRECATED pvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The date and time the policy was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvCreateDate :: Lens.Lens' PolicyVersion (Lude.Maybe Lude.Timestamp)
pvCreateDate = Lens.lens (createDate :: PolicyVersion -> Lude.Maybe Lude.Timestamp) (\s a -> s {createDate = a} :: PolicyVersion)
{-# DEPRECATED pvCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | Specifies whether the policy version is the default.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvIsDefaultVersion :: Lens.Lens' PolicyVersion (Lude.Maybe Lude.Bool)
pvIsDefaultVersion = Lens.lens (isDefaultVersion :: PolicyVersion -> Lude.Maybe Lude.Bool) (\s a -> s {isDefaultVersion = a} :: PolicyVersion)
{-# DEPRECATED pvIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

instance Lude.FromJSON PolicyVersion where
  parseJSON =
    Lude.withObject
      "PolicyVersion"
      ( \x ->
          PolicyVersion'
            Lude.<$> (x Lude..:? "versionId")
            Lude.<*> (x Lude..:? "createDate")
            Lude.<*> (x Lude..:? "isDefaultVersion")
      )
