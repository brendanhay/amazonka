{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.ContactFlow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.ContactFlow
  ( ContactFlow (..),

    -- * Smart constructor
    mkContactFlow,

    -- * Lenses
    cfARN,
    cfContent,
    cfName,
    cfId,
    cfType,
    cfDescription,
    cfTags,
  )
where

import Network.AWS.Connect.Types.ContactFlowType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a contact flow.
--
-- /See:/ 'mkContactFlow' smart constructor.
data ContactFlow = ContactFlow'
  { arn :: Lude.Maybe Lude.Text,
    content :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe ContactFlowType,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContactFlow' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the contact flow.
-- * 'content' - The content of the contact flow.
-- * 'description' - The description of the contact flow.
-- * 'id' - The identifier of the contact flow.
-- * 'name' - The name of the contact flow.
-- * 'tags' - One or more tags.
-- * 'type'' - The type of the contact flow. For descriptions of the available types, see <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a Contact Flow Type> in the /Amazon Connect Administrator Guide/ .
mkContactFlow ::
  ContactFlow
mkContactFlow =
  ContactFlow'
    { arn = Lude.Nothing,
      content = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the contact flow.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfARN :: Lens.Lens' ContactFlow (Lude.Maybe Lude.Text)
cfARN = Lens.lens (arn :: ContactFlow -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ContactFlow)
{-# DEPRECATED cfARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The content of the contact flow.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfContent :: Lens.Lens' ContactFlow (Lude.Maybe Lude.Text)
cfContent = Lens.lens (content :: ContactFlow -> Lude.Maybe Lude.Text) (\s a -> s {content = a} :: ContactFlow)
{-# DEPRECATED cfContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The name of the contact flow.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' ContactFlow (Lude.Maybe Lude.Text)
cfName = Lens.lens (name :: ContactFlow -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ContactFlow)
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the contact flow.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfId :: Lens.Lens' ContactFlow (Lude.Maybe Lude.Text)
cfId = Lens.lens (id :: ContactFlow -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ContactFlow)
{-# DEPRECATED cfId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of the contact flow. For descriptions of the available types, see <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a Contact Flow Type> in the /Amazon Connect Administrator Guide/ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfType :: Lens.Lens' ContactFlow (Lude.Maybe ContactFlowType)
cfType = Lens.lens (type' :: ContactFlow -> Lude.Maybe ContactFlowType) (\s a -> s {type' = a} :: ContactFlow)
{-# DEPRECATED cfType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The description of the contact flow.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDescription :: Lens.Lens' ContactFlow (Lude.Maybe Lude.Text)
cfDescription = Lens.lens (description :: ContactFlow -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ContactFlow)
{-# DEPRECATED cfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTags :: Lens.Lens' ContactFlow (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cfTags = Lens.lens (tags :: ContactFlow -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ContactFlow)
{-# DEPRECATED cfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON ContactFlow where
  parseJSON =
    Lude.withObject
      "ContactFlow"
      ( \x ->
          ContactFlow'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Content")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
