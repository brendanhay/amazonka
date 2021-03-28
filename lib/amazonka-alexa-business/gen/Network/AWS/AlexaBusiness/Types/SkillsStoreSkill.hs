{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SkillsStoreSkill
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.SkillsStoreSkill
  ( SkillsStoreSkill (..)
  -- * Smart constructor
  , mkSkillsStoreSkill
  -- * Lenses
  , sssIconUrl
  , sssSampleUtterances
  , sssShortDescription
  , sssSkillDetails
  , sssSkillId
  , sssSkillName
  , sssSupportsLinking
  ) where

import qualified Network.AWS.AlexaBusiness.Types.IconUrl as Types
import qualified Network.AWS.AlexaBusiness.Types.ShortDescription as Types
import qualified Network.AWS.AlexaBusiness.Types.SkillDetails as Types
import qualified Network.AWS.AlexaBusiness.Types.SkillId as Types
import qualified Network.AWS.AlexaBusiness.Types.SkillName as Types
import qualified Network.AWS.AlexaBusiness.Types.Utterance as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The detailed information about an Alexa skill.
--
-- /See:/ 'mkSkillsStoreSkill' smart constructor.
data SkillsStoreSkill = SkillsStoreSkill'
  { iconUrl :: Core.Maybe Types.IconUrl
    -- ^ The URL where the skill icon resides.
  , sampleUtterances :: Core.Maybe [Types.Utterance]
    -- ^ Sample utterances that interact with the skill.
  , shortDescription :: Core.Maybe Types.ShortDescription
    -- ^ Short description about the skill.
  , skillDetails :: Core.Maybe Types.SkillDetails
    -- ^ Information about the skill.
  , skillId :: Core.Maybe Types.SkillId
    -- ^ The ARN of the skill.
  , skillName :: Core.Maybe Types.SkillName
    -- ^ The name of the skill.
  , supportsLinking :: Core.Maybe Core.Bool
    -- ^ Linking support for a skill.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SkillsStoreSkill' value with any optional fields omitted.
mkSkillsStoreSkill
    :: SkillsStoreSkill
mkSkillsStoreSkill
  = SkillsStoreSkill'{iconUrl = Core.Nothing,
                      sampleUtterances = Core.Nothing, shortDescription = Core.Nothing,
                      skillDetails = Core.Nothing, skillId = Core.Nothing,
                      skillName = Core.Nothing, supportsLinking = Core.Nothing}

-- | The URL where the skill icon resides.
--
-- /Note:/ Consider using 'iconUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssIconUrl :: Lens.Lens' SkillsStoreSkill (Core.Maybe Types.IconUrl)
sssIconUrl = Lens.field @"iconUrl"
{-# INLINEABLE sssIconUrl #-}
{-# DEPRECATED iconUrl "Use generic-lens or generic-optics with 'iconUrl' instead"  #-}

-- | Sample utterances that interact with the skill.
--
-- /Note:/ Consider using 'sampleUtterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssSampleUtterances :: Lens.Lens' SkillsStoreSkill (Core.Maybe [Types.Utterance])
sssSampleUtterances = Lens.field @"sampleUtterances"
{-# INLINEABLE sssSampleUtterances #-}
{-# DEPRECATED sampleUtterances "Use generic-lens or generic-optics with 'sampleUtterances' instead"  #-}

-- | Short description about the skill.
--
-- /Note:/ Consider using 'shortDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssShortDescription :: Lens.Lens' SkillsStoreSkill (Core.Maybe Types.ShortDescription)
sssShortDescription = Lens.field @"shortDescription"
{-# INLINEABLE sssShortDescription #-}
{-# DEPRECATED shortDescription "Use generic-lens or generic-optics with 'shortDescription' instead"  #-}

-- | Information about the skill.
--
-- /Note:/ Consider using 'skillDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssSkillDetails :: Lens.Lens' SkillsStoreSkill (Core.Maybe Types.SkillDetails)
sssSkillDetails = Lens.field @"skillDetails"
{-# INLINEABLE sssSkillDetails #-}
{-# DEPRECATED skillDetails "Use generic-lens or generic-optics with 'skillDetails' instead"  #-}

-- | The ARN of the skill.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssSkillId :: Lens.Lens' SkillsStoreSkill (Core.Maybe Types.SkillId)
sssSkillId = Lens.field @"skillId"
{-# INLINEABLE sssSkillId #-}
{-# DEPRECATED skillId "Use generic-lens or generic-optics with 'skillId' instead"  #-}

-- | The name of the skill.
--
-- /Note:/ Consider using 'skillName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssSkillName :: Lens.Lens' SkillsStoreSkill (Core.Maybe Types.SkillName)
sssSkillName = Lens.field @"skillName"
{-# INLINEABLE sssSkillName #-}
{-# DEPRECATED skillName "Use generic-lens or generic-optics with 'skillName' instead"  #-}

-- | Linking support for a skill.
--
-- /Note:/ Consider using 'supportsLinking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssSupportsLinking :: Lens.Lens' SkillsStoreSkill (Core.Maybe Core.Bool)
sssSupportsLinking = Lens.field @"supportsLinking"
{-# INLINEABLE sssSupportsLinking #-}
{-# DEPRECATED supportsLinking "Use generic-lens or generic-optics with 'supportsLinking' instead"  #-}

instance Core.FromJSON SkillsStoreSkill where
        parseJSON
          = Core.withObject "SkillsStoreSkill" Core.$
              \ x ->
                SkillsStoreSkill' Core.<$>
                  (x Core..:? "IconUrl") Core.<*> x Core..:? "SampleUtterances"
                    Core.<*> x Core..:? "ShortDescription"
                    Core.<*> x Core..:? "SkillDetails"
                    Core.<*> x Core..:? "SkillId"
                    Core.<*> x Core..:? "SkillName"
                    Core.<*> x Core..:? "SupportsLinking"
