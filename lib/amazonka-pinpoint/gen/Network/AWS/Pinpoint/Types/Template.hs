-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Template
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Template
  ( Template (..),

    -- * Smart constructor
    mkTemplate,

    -- * Lenses
    tName,
    tVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the name and version of the message template to use for the message.
--
-- /See:/ 'mkTemplate' smart constructor.
data Template = Template'
  { name :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Template' with the minimum fields required to make a request.
--
-- * 'name' - The name of the message template to use for the message. If specified, this value must match the name of an existing message template.
-- * 'version' - The unique identifier for the version of the message template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
mkTemplate ::
  Template
mkTemplate = Template' {name = Lude.Nothing, version = Lude.Nothing}

-- | The name of the message template to use for the message. If specified, this value must match the name of an existing message template.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tName :: Lens.Lens' Template (Lude.Maybe Lude.Text)
tName = Lens.lens (name :: Template -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Template)
{-# DEPRECATED tName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier for the version of the message template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tVersion :: Lens.Lens' Template (Lude.Maybe Lude.Text)
tVersion = Lens.lens (version :: Template -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: Template)
{-# DEPRECATED tVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON Template where
  parseJSON =
    Lude.withObject
      "Template"
      ( \x ->
          Template'
            Lude.<$> (x Lude..:? "Name") Lude.<*> (x Lude..:? "Version")
      )

instance Lude.ToJSON Template where
  toJSON Template' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            ("Version" Lude..=) Lude.<$> version
          ]
      )
