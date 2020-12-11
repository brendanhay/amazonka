-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplateResponse
  ( TemplateResponse (..),

    -- * Smart constructor
    mkTemplateResponse,

    -- * Lenses
    temARN,
    temTemplateDescription,
    temDefaultSubstitutions,
    temVersion,
    temTags,
    temLastModifiedDate,
    temCreationDate,
    temTemplateName,
    temTemplateType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.TemplateType
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a message template that's associated with your Amazon Pinpoint account.
--
-- /See:/ 'mkTemplateResponse' smart constructor.
data TemplateResponse = TemplateResponse'
  { arn ::
      Lude.Maybe Lude.Text,
    templateDescription :: Lude.Maybe Lude.Text,
    defaultSubstitutions :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    lastModifiedDate :: Lude.Text,
    creationDate :: Lude.Text,
    templateName :: Lude.Text,
    templateType :: TemplateType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TemplateResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the message template. This value isn't included in a TemplateResponse object. To retrieve the ARN of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the ARN for.
-- * 'creationDate' - The date, in ISO 8601 format, when the message template was created.
-- * 'defaultSubstitutions' - The JSON object that specifies the default values that are used for message variables in the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
-- * 'lastModifiedDate' - The date, in ISO 8601 format, when the message template was last modified.
-- * 'tags' - A map of key-value pairs that identifies the tags that are associated with the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
-- * 'templateDescription' - The custom description of the message template. This value isn't included in a TemplateResponse object. To retrieve the description of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the description for.
-- * 'templateName' - The name of the message template.
-- * 'templateType' - The type of channel that the message template is designed for. Possible values are: EMAIL, PUSH, SMS, and VOICE.
-- * 'version' - The unique identifier, as an integer, for the active version of the message template.
mkTemplateResponse ::
  -- | 'lastModifiedDate'
  Lude.Text ->
  -- | 'creationDate'
  Lude.Text ->
  -- | 'templateName'
  Lude.Text ->
  -- | 'templateType'
  TemplateType ->
  TemplateResponse
mkTemplateResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateName_
  pTemplateType_ =
    TemplateResponse'
      { arn = Lude.Nothing,
        templateDescription = Lude.Nothing,
        defaultSubstitutions = Lude.Nothing,
        version = Lude.Nothing,
        tags = Lude.Nothing,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        templateName = pTemplateName_,
        templateType = pTemplateType_
      }

-- | The Amazon Resource Name (ARN) of the message template. This value isn't included in a TemplateResponse object. To retrieve the ARN of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the ARN for.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
temARN :: Lens.Lens' TemplateResponse (Lude.Maybe Lude.Text)
temARN = Lens.lens (arn :: TemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: TemplateResponse)
{-# DEPRECATED temARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The custom description of the message template. This value isn't included in a TemplateResponse object. To retrieve the description of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the description for.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
temTemplateDescription :: Lens.Lens' TemplateResponse (Lude.Maybe Lude.Text)
temTemplateDescription = Lens.lens (templateDescription :: TemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateDescription = a} :: TemplateResponse)
{-# DEPRECATED temTemplateDescription "Use generic-lens or generic-optics with 'templateDescription' instead." #-}

-- | The JSON object that specifies the default values that are used for message variables in the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
temDefaultSubstitutions :: Lens.Lens' TemplateResponse (Lude.Maybe Lude.Text)
temDefaultSubstitutions = Lens.lens (defaultSubstitutions :: TemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {defaultSubstitutions = a} :: TemplateResponse)
{-# DEPRECATED temDefaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead." #-}

-- | The unique identifier, as an integer, for the active version of the message template.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
temVersion :: Lens.Lens' TemplateResponse (Lude.Maybe Lude.Text)
temVersion = Lens.lens (version :: TemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: TemplateResponse)
{-# DEPRECATED temVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | A map of key-value pairs that identifies the tags that are associated with the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
temTags :: Lens.Lens' TemplateResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
temTags = Lens.lens (tags :: TemplateResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: TemplateResponse)
{-# DEPRECATED temTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The date, in ISO 8601 format, when the message template was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
temLastModifiedDate :: Lens.Lens' TemplateResponse Lude.Text
temLastModifiedDate = Lens.lens (lastModifiedDate :: TemplateResponse -> Lude.Text) (\s a -> s {lastModifiedDate = a} :: TemplateResponse)
{-# DEPRECATED temLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The date, in ISO 8601 format, when the message template was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
temCreationDate :: Lens.Lens' TemplateResponse Lude.Text
temCreationDate = Lens.lens (creationDate :: TemplateResponse -> Lude.Text) (\s a -> s {creationDate = a} :: TemplateResponse)
{-# DEPRECATED temCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The name of the message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
temTemplateName :: Lens.Lens' TemplateResponse Lude.Text
temTemplateName = Lens.lens (templateName :: TemplateResponse -> Lude.Text) (\s a -> s {templateName = a} :: TemplateResponse)
{-# DEPRECATED temTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The type of channel that the message template is designed for. Possible values are: EMAIL, PUSH, SMS, and VOICE.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
temTemplateType :: Lens.Lens' TemplateResponse TemplateType
temTemplateType = Lens.lens (templateType :: TemplateResponse -> TemplateType) (\s a -> s {templateType = a} :: TemplateResponse)
{-# DEPRECATED temTemplateType "Use generic-lens or generic-optics with 'templateType' instead." #-}

instance Lude.FromJSON TemplateResponse where
  parseJSON =
    Lude.withObject
      "TemplateResponse"
      ( \x ->
          TemplateResponse'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "TemplateDescription")
            Lude.<*> (x Lude..:? "DefaultSubstitutions")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "LastModifiedDate")
            Lude.<*> (x Lude..: "CreationDate")
            Lude.<*> (x Lude..: "TemplateName")
            Lude.<*> (x Lude..: "TemplateType")
      )
