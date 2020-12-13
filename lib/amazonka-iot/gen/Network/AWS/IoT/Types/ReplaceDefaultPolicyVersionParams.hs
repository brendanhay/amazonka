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

import Network.AWS.IoT.Types.PolicyTemplateName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Parameters to define a mitigation action that adds a blank policy to restrict permissions.
--
-- /See:/ 'mkReplaceDefaultPolicyVersionParams' smart constructor.
newtype ReplaceDefaultPolicyVersionParams = ReplaceDefaultPolicyVersionParams'
  { -- | The name of the template to be applied. The only supported value is @BLANK_POLICY@ .
    templateName :: PolicyTemplateName
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplaceDefaultPolicyVersionParams' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the template to be applied. The only supported value is @BLANK_POLICY@ .
mkReplaceDefaultPolicyVersionParams ::
  -- | 'templateName'
  PolicyTemplateName ->
  ReplaceDefaultPolicyVersionParams
mkReplaceDefaultPolicyVersionParams pTemplateName_ =
  ReplaceDefaultPolicyVersionParams' {templateName = pTemplateName_}

-- | The name of the template to be applied. The only supported value is @BLANK_POLICY@ .
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpvpTemplateName :: Lens.Lens' ReplaceDefaultPolicyVersionParams PolicyTemplateName
rdpvpTemplateName = Lens.lens (templateName :: ReplaceDefaultPolicyVersionParams -> PolicyTemplateName) (\s a -> s {templateName = a} :: ReplaceDefaultPolicyVersionParams)
{-# DEPRECATED rdpvpTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Lude.FromJSON ReplaceDefaultPolicyVersionParams where
  parseJSON =
    Lude.withObject
      "ReplaceDefaultPolicyVersionParams"
      ( \x ->
          ReplaceDefaultPolicyVersionParams'
            Lude.<$> (x Lude..: "templateName")
      )

instance Lude.ToJSON ReplaceDefaultPolicyVersionParams where
  toJSON ReplaceDefaultPolicyVersionParams' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("templateName" Lude..= templateName)])
