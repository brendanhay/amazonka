{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ReplaceDefaultPolicyVersionParams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ReplaceDefaultPolicyVersionParams
  ( ReplaceDefaultPolicyVersionParams (..),

    -- * Smart constructor
    mkReplaceDefaultPolicyVersionParams,

    -- * Lenses
    rdpvpTemplateName,
  )
where

import qualified Network.AWS.IoT.Types.PolicyTemplateName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Parameters to define a mitigation action that adds a blank policy to restrict permissions.
--
-- /See:/ 'mkReplaceDefaultPolicyVersionParams' smart constructor.
newtype ReplaceDefaultPolicyVersionParams = ReplaceDefaultPolicyVersionParams'
  { -- | The name of the template to be applied. The only supported value is @BLANK_POLICY@ .
    templateName :: Types.PolicyTemplateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceDefaultPolicyVersionParams' value with any optional fields omitted.
mkReplaceDefaultPolicyVersionParams ::
  -- | 'templateName'
  Types.PolicyTemplateName ->
  ReplaceDefaultPolicyVersionParams
mkReplaceDefaultPolicyVersionParams templateName =
  ReplaceDefaultPolicyVersionParams' {templateName}

-- | The name of the template to be applied. The only supported value is @BLANK_POLICY@ .
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpvpTemplateName :: Lens.Lens' ReplaceDefaultPolicyVersionParams Types.PolicyTemplateName
rdpvpTemplateName = Lens.field @"templateName"
{-# DEPRECATED rdpvpTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Core.FromJSON ReplaceDefaultPolicyVersionParams where
  toJSON ReplaceDefaultPolicyVersionParams {..} =
    Core.object
      (Core.catMaybes [Core.Just ("templateName" Core..= templateName)])

instance Core.FromJSON ReplaceDefaultPolicyVersionParams where
  parseJSON =
    Core.withObject "ReplaceDefaultPolicyVersionParams" Core.$
      \x ->
        ReplaceDefaultPolicyVersionParams'
          Core.<$> (x Core..: "templateName")
