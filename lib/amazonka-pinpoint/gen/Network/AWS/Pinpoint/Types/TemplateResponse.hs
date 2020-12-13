{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    tfTemplateName,
    tfLastModifiedDate,
    tfARN,
    tfTemplateType,
    tfTemplateDescription,
    tfDefaultSubstitutions,
    tfVersion,
    tfCreationDate,
    tfTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.TemplateType
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a message template that's associated with your Amazon Pinpoint account.
--
-- /See:/ 'mkTemplateResponse' smart constructor.
data TemplateResponse = TemplateResponse'
  { -- | The name of the message template.
    templateName :: Lude.Text,
    -- | The date, in ISO 8601 format, when the message template was last modified.
    lastModifiedDate :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the message template. This value isn't included in a TemplateResponse object. To retrieve the ARN of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the ARN for.
    arn :: Lude.Maybe Lude.Text,
    -- | The type of channel that the message template is designed for. Possible values are: EMAIL, PUSH, SMS, and VOICE.
    templateType :: TemplateType,
    -- | The custom description of the message template. This value isn't included in a TemplateResponse object. To retrieve the description of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the description for.
    templateDescription :: Lude.Maybe Lude.Text,
    -- | The JSON object that specifies the default values that are used for message variables in the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
    defaultSubstitutions :: Lude.Maybe Lude.Text,
    -- | The unique identifier, as an integer, for the active version of the message template.
    version :: Lude.Maybe Lude.Text,
    -- | The date, in ISO 8601 format, when the message template was created.
    creationDate :: Lude.Text,
    -- | A map of key-value pairs that identifies the tags that are associated with the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TemplateResponse' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the message template.
-- * 'lastModifiedDate' - The date, in ISO 8601 format, when the message template was last modified.
-- * 'arn' - The Amazon Resource Name (ARN) of the message template. This value isn't included in a TemplateResponse object. To retrieve the ARN of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the ARN for.
-- * 'templateType' - The type of channel that the message template is designed for. Possible values are: EMAIL, PUSH, SMS, and VOICE.
-- * 'templateDescription' - The custom description of the message template. This value isn't included in a TemplateResponse object. To retrieve the description of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the description for.
-- * 'defaultSubstitutions' - The JSON object that specifies the default values that are used for message variables in the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
-- * 'version' - The unique identifier, as an integer, for the active version of the message template.
-- * 'creationDate' - The date, in ISO 8601 format, when the message template was created.
-- * 'tags' - A map of key-value pairs that identifies the tags that are associated with the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
mkTemplateResponse ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'lastModifiedDate'
  Lude.Text ->
  -- | 'templateType'
  TemplateType ->
  -- | 'creationDate'
  Lude.Text ->
  TemplateResponse
mkTemplateResponse
  pTemplateName_
  pLastModifiedDate_
  pTemplateType_
  pCreationDate_ =
    TemplateResponse'
      { templateName = pTemplateName_,
        lastModifiedDate = pLastModifiedDate_,
        arn = Lude.Nothing,
        templateType = pTemplateType_,
        templateDescription = Lude.Nothing,
        defaultSubstitutions = Lude.Nothing,
        version = Lude.Nothing,
        creationDate = pCreationDate_,
        tags = Lude.Nothing
      }

-- | The name of the message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfTemplateName :: Lens.Lens' TemplateResponse Lude.Text
tfTemplateName = Lens.lens (templateName :: TemplateResponse -> Lude.Text) (\s a -> s {templateName = a} :: TemplateResponse)
{-# DEPRECATED tfTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The date, in ISO 8601 format, when the message template was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfLastModifiedDate :: Lens.Lens' TemplateResponse Lude.Text
tfLastModifiedDate = Lens.lens (lastModifiedDate :: TemplateResponse -> Lude.Text) (\s a -> s {lastModifiedDate = a} :: TemplateResponse)
{-# DEPRECATED tfLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The Amazon Resource Name (ARN) of the message template. This value isn't included in a TemplateResponse object. To retrieve the ARN of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the ARN for.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfARN :: Lens.Lens' TemplateResponse (Lude.Maybe Lude.Text)
tfARN = Lens.lens (arn :: TemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: TemplateResponse)
{-# DEPRECATED tfARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The type of channel that the message template is designed for. Possible values are: EMAIL, PUSH, SMS, and VOICE.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfTemplateType :: Lens.Lens' TemplateResponse TemplateType
tfTemplateType = Lens.lens (templateType :: TemplateResponse -> TemplateType) (\s a -> s {templateType = a} :: TemplateResponse)
{-# DEPRECATED tfTemplateType "Use generic-lens or generic-optics with 'templateType' instead." #-}

-- | The custom description of the message template. This value isn't included in a TemplateResponse object. To retrieve the description of a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the description for.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfTemplateDescription :: Lens.Lens' TemplateResponse (Lude.Maybe Lude.Text)
tfTemplateDescription = Lens.lens (templateDescription :: TemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateDescription = a} :: TemplateResponse)
{-# DEPRECATED tfTemplateDescription "Use generic-lens or generic-optics with 'templateDescription' instead." #-}

-- | The JSON object that specifies the default values that are used for message variables in the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfDefaultSubstitutions :: Lens.Lens' TemplateResponse (Lude.Maybe Lude.Text)
tfDefaultSubstitutions = Lens.lens (defaultSubstitutions :: TemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {defaultSubstitutions = a} :: TemplateResponse)
{-# DEPRECATED tfDefaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead." #-}

-- | The unique identifier, as an integer, for the active version of the message template.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfVersion :: Lens.Lens' TemplateResponse (Lude.Maybe Lude.Text)
tfVersion = Lens.lens (version :: TemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: TemplateResponse)
{-# DEPRECATED tfVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The date, in ISO 8601 format, when the message template was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfCreationDate :: Lens.Lens' TemplateResponse Lude.Text
tfCreationDate = Lens.lens (creationDate :: TemplateResponse -> Lude.Text) (\s a -> s {creationDate = a} :: TemplateResponse)
{-# DEPRECATED tfCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | A map of key-value pairs that identifies the tags that are associated with the message template. This object isn't included in a TemplateResponse object. To retrieve this object for a template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate operation, depending on the type of template that you want to retrieve the object for.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfTags :: Lens.Lens' TemplateResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
tfTags = Lens.lens (tags :: TemplateResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: TemplateResponse)
{-# DEPRECATED tfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON TemplateResponse where
  parseJSON =
    Lude.withObject
      "TemplateResponse"
      ( \x ->
          TemplateResponse'
            Lude.<$> (x Lude..: "TemplateName")
            Lude.<*> (x Lude..: "LastModifiedDate")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..: "TemplateType")
            Lude.<*> (x Lude..:? "TemplateDescription")
            Lude.<*> (x Lude..:? "DefaultSubstitutions")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..: "CreationDate")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
