-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateVersionResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplateVersionResponse
  ( TemplateVersionResponse (..),

    -- * Smart constructor
    mkTemplateVersionResponse,

    -- * Lenses
    tvTemplateDescription,
    tvDefaultSubstitutions,
    tvVersion,
    tvLastModifiedDate,
    tvCreationDate,
    tvTemplateName,
    tvTemplateType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a specific version of a message template.
--
-- /See:/ 'mkTemplateVersionResponse' smart constructor.
data TemplateVersionResponse = TemplateVersionResponse'
  { templateDescription ::
      Lude.Maybe Lude.Text,
    defaultSubstitutions ::
      Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    lastModifiedDate :: Lude.Text,
    creationDate :: Lude.Text,
    templateName :: Lude.Text,
    templateType :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TemplateVersionResponse' with the minimum fields required to make a request.
--
-- * 'creationDate' - The date, in ISO 8601 format, when the version of the message template was created.
-- * 'defaultSubstitutions' - A JSON object that specifies the default values that are used for message variables in the version of the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
-- * 'lastModifiedDate' - The date, in ISO 8601 format, when the version of the message template was last modified.
-- * 'templateDescription' - The custom description of the version of the message template.
-- * 'templateName' - The name of the message template.
-- * 'templateType' - The type of channel that the message template is designed for. Possible values are: EMAIL, PUSH, SMS, and VOICE.
-- * 'version' - The unique identifier for the version of the message template. This value is an integer that Amazon Pinpoint automatically increments and assigns to each new version of a template.
mkTemplateVersionResponse ::
  -- | 'lastModifiedDate'
  Lude.Text ->
  -- | 'creationDate'
  Lude.Text ->
  -- | 'templateName'
  Lude.Text ->
  -- | 'templateType'
  Lude.Text ->
  TemplateVersionResponse
mkTemplateVersionResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateName_
  pTemplateType_ =
    TemplateVersionResponse'
      { templateDescription = Lude.Nothing,
        defaultSubstitutions = Lude.Nothing,
        version = Lude.Nothing,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        templateName = pTemplateName_,
        templateType = pTemplateType_
      }

-- | The custom description of the version of the message template.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvTemplateDescription :: Lens.Lens' TemplateVersionResponse (Lude.Maybe Lude.Text)
tvTemplateDescription = Lens.lens (templateDescription :: TemplateVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateDescription = a} :: TemplateVersionResponse)
{-# DEPRECATED tvTemplateDescription "Use generic-lens or generic-optics with 'templateDescription' instead." #-}

-- | A JSON object that specifies the default values that are used for message variables in the version of the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvDefaultSubstitutions :: Lens.Lens' TemplateVersionResponse (Lude.Maybe Lude.Text)
tvDefaultSubstitutions = Lens.lens (defaultSubstitutions :: TemplateVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {defaultSubstitutions = a} :: TemplateVersionResponse)
{-# DEPRECATED tvDefaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead." #-}

-- | The unique identifier for the version of the message template. This value is an integer that Amazon Pinpoint automatically increments and assigns to each new version of a template.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvVersion :: Lens.Lens' TemplateVersionResponse (Lude.Maybe Lude.Text)
tvVersion = Lens.lens (version :: TemplateVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: TemplateVersionResponse)
{-# DEPRECATED tvVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The date, in ISO 8601 format, when the version of the message template was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvLastModifiedDate :: Lens.Lens' TemplateVersionResponse Lude.Text
tvLastModifiedDate = Lens.lens (lastModifiedDate :: TemplateVersionResponse -> Lude.Text) (\s a -> s {lastModifiedDate = a} :: TemplateVersionResponse)
{-# DEPRECATED tvLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The date, in ISO 8601 format, when the version of the message template was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvCreationDate :: Lens.Lens' TemplateVersionResponse Lude.Text
tvCreationDate = Lens.lens (creationDate :: TemplateVersionResponse -> Lude.Text) (\s a -> s {creationDate = a} :: TemplateVersionResponse)
{-# DEPRECATED tvCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The name of the message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvTemplateName :: Lens.Lens' TemplateVersionResponse Lude.Text
tvTemplateName = Lens.lens (templateName :: TemplateVersionResponse -> Lude.Text) (\s a -> s {templateName = a} :: TemplateVersionResponse)
{-# DEPRECATED tvTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The type of channel that the message template is designed for. Possible values are: EMAIL, PUSH, SMS, and VOICE.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvTemplateType :: Lens.Lens' TemplateVersionResponse Lude.Text
tvTemplateType = Lens.lens (templateType :: TemplateVersionResponse -> Lude.Text) (\s a -> s {templateType = a} :: TemplateVersionResponse)
{-# DEPRECATED tvTemplateType "Use generic-lens or generic-optics with 'templateType' instead." #-}

instance Lude.FromJSON TemplateVersionResponse where
  parseJSON =
    Lude.withObject
      "TemplateVersionResponse"
      ( \x ->
          TemplateVersionResponse'
            Lude.<$> (x Lude..:? "TemplateDescription")
            Lude.<*> (x Lude..:? "DefaultSubstitutions")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..: "LastModifiedDate")
            Lude.<*> (x Lude..: "CreationDate")
            Lude.<*> (x Lude..: "TemplateName")
            Lude.<*> (x Lude..: "TemplateType")
      )
