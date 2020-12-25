{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    tvrLastModifiedDate,
    tvrCreationDate,
    tvrTemplateName,
    tvrTemplateType,
    tvrDefaultSubstitutions,
    tvrTemplateDescription,
    tvrVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about a specific version of a message template.
--
-- /See:/ 'mkTemplateVersionResponse' smart constructor.
data TemplateVersionResponse = TemplateVersionResponse'
  { -- | The date, in ISO 8601 format, when the version of the message template was last modified.
    lastModifiedDate :: Core.Text,
    -- | The date, in ISO 8601 format, when the version of the message template was created.
    creationDate :: Core.Text,
    -- | The name of the message template.
    templateName :: Core.Text,
    -- | The type of channel that the message template is designed for. Possible values are: EMAIL, PUSH, SMS, and VOICE.
    templateType :: Core.Text,
    -- | A JSON object that specifies the default values that are used for message variables in the version of the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
    defaultSubstitutions :: Core.Maybe Core.Text,
    -- | The custom description of the version of the message template.
    templateDescription :: Core.Maybe Core.Text,
    -- | The unique identifier for the version of the message template. This value is an integer that Amazon Pinpoint automatically increments and assigns to each new version of a template.
    version :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TemplateVersionResponse' value with any optional fields omitted.
mkTemplateVersionResponse ::
  -- | 'lastModifiedDate'
  Core.Text ->
  -- | 'creationDate'
  Core.Text ->
  -- | 'templateName'
  Core.Text ->
  -- | 'templateType'
  Core.Text ->
  TemplateVersionResponse
mkTemplateVersionResponse
  lastModifiedDate
  creationDate
  templateName
  templateType =
    TemplateVersionResponse'
      { lastModifiedDate,
        creationDate,
        templateName,
        templateType,
        defaultSubstitutions = Core.Nothing,
        templateDescription = Core.Nothing,
        version = Core.Nothing
      }

-- | The date, in ISO 8601 format, when the version of the message template was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvrLastModifiedDate :: Lens.Lens' TemplateVersionResponse Core.Text
tvrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED tvrLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The date, in ISO 8601 format, when the version of the message template was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvrCreationDate :: Lens.Lens' TemplateVersionResponse Core.Text
tvrCreationDate = Lens.field @"creationDate"
{-# DEPRECATED tvrCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The name of the message template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvrTemplateName :: Lens.Lens' TemplateVersionResponse Core.Text
tvrTemplateName = Lens.field @"templateName"
{-# DEPRECATED tvrTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The type of channel that the message template is designed for. Possible values are: EMAIL, PUSH, SMS, and VOICE.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvrTemplateType :: Lens.Lens' TemplateVersionResponse Core.Text
tvrTemplateType = Lens.field @"templateType"
{-# DEPRECATED tvrTemplateType "Use generic-lens or generic-optics with 'templateType' instead." #-}

-- | A JSON object that specifies the default values that are used for message variables in the version of the message template. This object is a set of key-value pairs. Each key defines a message variable in the template. The corresponding value defines the default value for that variable.
--
-- /Note:/ Consider using 'defaultSubstitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvrDefaultSubstitutions :: Lens.Lens' TemplateVersionResponse (Core.Maybe Core.Text)
tvrDefaultSubstitutions = Lens.field @"defaultSubstitutions"
{-# DEPRECATED tvrDefaultSubstitutions "Use generic-lens or generic-optics with 'defaultSubstitutions' instead." #-}

-- | The custom description of the version of the message template.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvrTemplateDescription :: Lens.Lens' TemplateVersionResponse (Core.Maybe Core.Text)
tvrTemplateDescription = Lens.field @"templateDescription"
{-# DEPRECATED tvrTemplateDescription "Use generic-lens or generic-optics with 'templateDescription' instead." #-}

-- | The unique identifier for the version of the message template. This value is an integer that Amazon Pinpoint automatically increments and assigns to each new version of a template.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvrVersion :: Lens.Lens' TemplateVersionResponse (Core.Maybe Core.Text)
tvrVersion = Lens.field @"version"
{-# DEPRECATED tvrVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON TemplateVersionResponse where
  parseJSON =
    Core.withObject "TemplateVersionResponse" Core.$
      \x ->
        TemplateVersionResponse'
          Core.<$> (x Core..: "LastModifiedDate")
          Core.<*> (x Core..: "CreationDate")
          Core.<*> (x Core..: "TemplateName")
          Core.<*> (x Core..: "TemplateType")
          Core.<*> (x Core..:? "DefaultSubstitutions")
          Core.<*> (x Core..:? "TemplateDescription")
          Core.<*> (x Core..:? "Version")
