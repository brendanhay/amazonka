{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingRuleRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingRuleRecord
  ( SamplingRuleRecord (..),

    -- * Smart constructor
    mkSamplingRuleRecord,

    -- * Lenses
    srrModifiedAt,
    srrSamplingRule,
    srrCreatedAt,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.SamplingRule

-- | A 'SamplingRule' and its metadata.
--
-- /See:/ 'mkSamplingRuleRecord' smart constructor.
data SamplingRuleRecord = SamplingRuleRecord'
  { -- | When the rule was last modified.
    modifiedAt :: Lude.Maybe Lude.Timestamp,
    -- | The sampling rule.
    samplingRule :: Lude.Maybe SamplingRule,
    -- | When the rule was created.
    createdAt :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SamplingRuleRecord' with the minimum fields required to make a request.
--
-- * 'modifiedAt' - When the rule was last modified.
-- * 'samplingRule' - The sampling rule.
-- * 'createdAt' - When the rule was created.
mkSamplingRuleRecord ::
  SamplingRuleRecord
mkSamplingRuleRecord =
  SamplingRuleRecord'
    { modifiedAt = Lude.Nothing,
      samplingRule = Lude.Nothing,
      createdAt = Lude.Nothing
    }

-- | When the rule was last modified.
--
-- /Note:/ Consider using 'modifiedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrModifiedAt :: Lens.Lens' SamplingRuleRecord (Lude.Maybe Lude.Timestamp)
srrModifiedAt = Lens.lens (modifiedAt :: SamplingRuleRecord -> Lude.Maybe Lude.Timestamp) (\s a -> s {modifiedAt = a} :: SamplingRuleRecord)
{-# DEPRECATED srrModifiedAt "Use generic-lens or generic-optics with 'modifiedAt' instead." #-}

-- | The sampling rule.
--
-- /Note:/ Consider using 'samplingRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrSamplingRule :: Lens.Lens' SamplingRuleRecord (Lude.Maybe SamplingRule)
srrSamplingRule = Lens.lens (samplingRule :: SamplingRuleRecord -> Lude.Maybe SamplingRule) (\s a -> s {samplingRule = a} :: SamplingRuleRecord)
{-# DEPRECATED srrSamplingRule "Use generic-lens or generic-optics with 'samplingRule' instead." #-}

-- | When the rule was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrCreatedAt :: Lens.Lens' SamplingRuleRecord (Lude.Maybe Lude.Timestamp)
srrCreatedAt = Lens.lens (createdAt :: SamplingRuleRecord -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: SamplingRuleRecord)
{-# DEPRECATED srrCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

instance Lude.FromJSON SamplingRuleRecord where
  parseJSON =
    Lude.withObject
      "SamplingRuleRecord"
      ( \x ->
          SamplingRuleRecord'
            Lude.<$> (x Lude..:? "ModifiedAt")
            Lude.<*> (x Lude..:? "SamplingRule")
            Lude.<*> (x Lude..:? "CreatedAt")
      )
