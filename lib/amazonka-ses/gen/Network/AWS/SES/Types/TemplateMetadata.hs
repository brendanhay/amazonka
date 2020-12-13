{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.TemplateMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.TemplateMetadata
  ( TemplateMetadata (..),

    -- * Smart constructor
    mkTemplateMetadata,

    -- * Lenses
    tmName,
    tmCreatedTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an email template.
--
-- /See:/ 'mkTemplateMetadata' smart constructor.
data TemplateMetadata = TemplateMetadata'
  { -- | The name of the template.
    name :: Lude.Maybe Lude.Text,
    -- | The time and date the template was created.
    createdTimestamp :: Lude.Maybe Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TemplateMetadata' with the minimum fields required to make a request.
--
-- * 'name' - The name of the template.
-- * 'createdTimestamp' - The time and date the template was created.
mkTemplateMetadata ::
  TemplateMetadata
mkTemplateMetadata =
  TemplateMetadata'
    { name = Lude.Nothing,
      createdTimestamp = Lude.Nothing
    }

-- | The name of the template.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmName :: Lens.Lens' TemplateMetadata (Lude.Maybe Lude.Text)
tmName = Lens.lens (name :: TemplateMetadata -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: TemplateMetadata)
{-# DEPRECATED tmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time and date the template was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmCreatedTimestamp :: Lens.Lens' TemplateMetadata (Lude.Maybe Lude.DateTime)
tmCreatedTimestamp = Lens.lens (createdTimestamp :: TemplateMetadata -> Lude.Maybe Lude.DateTime) (\s a -> s {createdTimestamp = a} :: TemplateMetadata)
{-# DEPRECATED tmCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

instance Lude.FromXML TemplateMetadata where
  parseXML x =
    TemplateMetadata'
      Lude.<$> (x Lude..@? "Name") Lude.<*> (x Lude..@? "CreatedTimestamp")
