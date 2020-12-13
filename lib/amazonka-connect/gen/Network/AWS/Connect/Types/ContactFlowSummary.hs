{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.ContactFlowSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.ContactFlowSummary
  ( ContactFlowSummary (..),

    -- * Smart constructor
    mkContactFlowSummary,

    -- * Lenses
    cfsARN,
    cfsName,
    cfsContactFlowType,
    cfsId,
  )
where

import Network.AWS.Connect.Types.ContactFlowType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains summary information about a contact flow.
--
-- You can also create and update contact flows using the <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language> .
--
-- /See:/ 'mkContactFlowSummary' smart constructor.
data ContactFlowSummary = ContactFlowSummary'
  { -- | The Amazon Resource Name (ARN) of the contact flow.
    arn :: Lude.Maybe Lude.Text,
    -- | The name of the contact flow.
    name :: Lude.Maybe Lude.Text,
    -- | The type of contact flow.
    contactFlowType :: Lude.Maybe ContactFlowType,
    -- | The identifier of the contact flow.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContactFlowSummary' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the contact flow.
-- * 'name' - The name of the contact flow.
-- * 'contactFlowType' - The type of contact flow.
-- * 'id' - The identifier of the contact flow.
mkContactFlowSummary ::
  ContactFlowSummary
mkContactFlowSummary =
  ContactFlowSummary'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      contactFlowType = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the contact flow.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsARN :: Lens.Lens' ContactFlowSummary (Lude.Maybe Lude.Text)
cfsARN = Lens.lens (arn :: ContactFlowSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ContactFlowSummary)
{-# DEPRECATED cfsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the contact flow.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsName :: Lens.Lens' ContactFlowSummary (Lude.Maybe Lude.Text)
cfsName = Lens.lens (name :: ContactFlowSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ContactFlowSummary)
{-# DEPRECATED cfsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of contact flow.
--
-- /Note:/ Consider using 'contactFlowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsContactFlowType :: Lens.Lens' ContactFlowSummary (Lude.Maybe ContactFlowType)
cfsContactFlowType = Lens.lens (contactFlowType :: ContactFlowSummary -> Lude.Maybe ContactFlowType) (\s a -> s {contactFlowType = a} :: ContactFlowSummary)
{-# DEPRECATED cfsContactFlowType "Use generic-lens or generic-optics with 'contactFlowType' instead." #-}

-- | The identifier of the contact flow.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsId :: Lens.Lens' ContactFlowSummary (Lude.Maybe Lude.Text)
cfsId = Lens.lens (id :: ContactFlowSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ContactFlowSummary)
{-# DEPRECATED cfsId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON ContactFlowSummary where
  parseJSON =
    Lude.withObject
      "ContactFlowSummary"
      ( \x ->
          ContactFlowSummary'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "ContactFlowType")
            Lude.<*> (x Lude..:? "Id")
      )
