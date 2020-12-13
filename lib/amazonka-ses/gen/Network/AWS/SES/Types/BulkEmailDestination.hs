{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BulkEmailDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BulkEmailDestination
  ( BulkEmailDestination (..),

    -- * Smart constructor
    mkBulkEmailDestination,

    -- * Lenses
    bedDestination,
    bedReplacementTemplateData,
    bedReplacementTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.Destination
import Network.AWS.SES.Types.MessageTag

-- | An array that contains one or more Destinations, as well as the tags and replacement data associated with each of those Destinations.
--
-- /See:/ 'mkBulkEmailDestination' smart constructor.
data BulkEmailDestination = BulkEmailDestination'
  { destination :: Destination,
    -- | A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
    replacementTemplateData :: Lude.Maybe Lude.Text,
    -- | A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendBulkTemplatedEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
    replacementTags :: Lude.Maybe [MessageTag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BulkEmailDestination' with the minimum fields required to make a request.
--
-- * 'destination' -
-- * 'replacementTemplateData' - A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
-- * 'replacementTags' - A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendBulkTemplatedEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
mkBulkEmailDestination ::
  -- | 'destination'
  Destination ->
  BulkEmailDestination
mkBulkEmailDestination pDestination_ =
  BulkEmailDestination'
    { destination = pDestination_,
      replacementTemplateData = Lude.Nothing,
      replacementTags = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bedDestination :: Lens.Lens' BulkEmailDestination Destination
bedDestination = Lens.lens (destination :: BulkEmailDestination -> Destination) (\s a -> s {destination = a} :: BulkEmailDestination)
{-# DEPRECATED bedDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
--
-- /Note:/ Consider using 'replacementTemplateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bedReplacementTemplateData :: Lens.Lens' BulkEmailDestination (Lude.Maybe Lude.Text)
bedReplacementTemplateData = Lens.lens (replacementTemplateData :: BulkEmailDestination -> Lude.Maybe Lude.Text) (\s a -> s {replacementTemplateData = a} :: BulkEmailDestination)
{-# DEPRECATED bedReplacementTemplateData "Use generic-lens or generic-optics with 'replacementTemplateData' instead." #-}

-- | A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendBulkTemplatedEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
--
-- /Note:/ Consider using 'replacementTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bedReplacementTags :: Lens.Lens' BulkEmailDestination (Lude.Maybe [MessageTag])
bedReplacementTags = Lens.lens (replacementTags :: BulkEmailDestination -> Lude.Maybe [MessageTag]) (\s a -> s {replacementTags = a} :: BulkEmailDestination)
{-# DEPRECATED bedReplacementTags "Use generic-lens or generic-optics with 'replacementTags' instead." #-}

instance Lude.ToQuery BulkEmailDestination where
  toQuery BulkEmailDestination' {..} =
    Lude.mconcat
      [ "Destination" Lude.=: destination,
        "ReplacementTemplateData" Lude.=: replacementTemplateData,
        "ReplacementTags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> replacementTags)
      ]
