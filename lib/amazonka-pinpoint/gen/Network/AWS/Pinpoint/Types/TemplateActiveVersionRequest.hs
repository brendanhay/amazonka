{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateActiveVersionRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplateActiveVersionRequest
  ( TemplateActiveVersionRequest (..),

    -- * Smart constructor
    mkTemplateActiveVersionRequest,

    -- * Lenses
    tavrVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies which version of a message template to use as the active version of the template.
--
-- /See:/ 'mkTemplateActiveVersionRequest' smart constructor.
newtype TemplateActiveVersionRequest = TemplateActiveVersionRequest'
  { -- | The version of the message template to use as the active version of the template. Valid values are: latest, for the most recent version of the template; or, the unique identifier for any existing version of the template. If you specify an identifier, the value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
    version :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TemplateActiveVersionRequest' with the minimum fields required to make a request.
--
-- * 'version' - The version of the message template to use as the active version of the template. Valid values are: latest, for the most recent version of the template; or, the unique identifier for any existing version of the template. If you specify an identifier, the value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
mkTemplateActiveVersionRequest ::
  TemplateActiveVersionRequest
mkTemplateActiveVersionRequest =
  TemplateActiveVersionRequest' {version = Lude.Nothing}

-- | The version of the message template to use as the active version of the template. Valid values are: latest, for the most recent version of the template; or, the unique identifier for any existing version of the template. If you specify an identifier, the value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavrVersion :: Lens.Lens' TemplateActiveVersionRequest (Lude.Maybe Lude.Text)
tavrVersion = Lens.lens (version :: TemplateActiveVersionRequest -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: TemplateActiveVersionRequest)
{-# DEPRECATED tavrVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.ToJSON TemplateActiveVersionRequest where
  toJSON TemplateActiveVersionRequest' {..} =
    Lude.object
      (Lude.catMaybes [("Version" Lude..=) Lude.<$> version])
