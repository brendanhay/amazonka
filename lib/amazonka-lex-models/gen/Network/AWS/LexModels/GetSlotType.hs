{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetSlotType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific version of a slot type. In addition to specifying the slot type name, you must specify the slot type version.
--
-- This operation requires permissions for the @lex:GetSlotType@ action.
module Network.AWS.LexModels.GetSlotType
  ( -- * Creating a request
    GetSlotType (..),
    mkGetSlotType,

    -- ** Request lenses
    gstName,
    gstVersion,

    -- * Destructuring the response
    GetSlotTypeResponse (..),
    mkGetSlotTypeResponse,

    -- ** Response lenses
    gstfrsParentSlotTypeSignature,
    gstfrsSlotTypeConfigurations,
    gstfrsChecksum,
    gstfrsValueSelectionStrategy,
    gstfrsCreatedDate,
    gstfrsName,
    gstfrsVersion,
    gstfrsLastUpdatedDate,
    gstfrsDescription,
    gstfrsEnumerationValues,
    gstfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSlotType' smart constructor.
data GetSlotType = GetSlotType'
  { -- | The name of the slot type. The name is case sensitive.
    name :: Lude.Text,
    -- | The version of the slot type.
    version :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSlotType' with the minimum fields required to make a request.
--
-- * 'name' - The name of the slot type. The name is case sensitive.
-- * 'version' - The version of the slot type.
mkGetSlotType ::
  -- | 'name'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  GetSlotType
mkGetSlotType pName_ pVersion_ =
  GetSlotType' {name = pName_, version = pVersion_}

-- | The name of the slot type. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstName :: Lens.Lens' GetSlotType Lude.Text
gstName = Lens.lens (name :: GetSlotType -> Lude.Text) (\s a -> s {name = a} :: GetSlotType)
{-# DEPRECATED gstName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the slot type.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstVersion :: Lens.Lens' GetSlotType Lude.Text
gstVersion = Lens.lens (version :: GetSlotType -> Lude.Text) (\s a -> s {version = a} :: GetSlotType)
{-# DEPRECATED gstVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest GetSlotType where
  type Rs GetSlotType = GetSlotTypeResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSlotTypeResponse'
            Lude.<$> (x Lude..?> "parentSlotTypeSignature")
            Lude.<*> (x Lude..?> "slotTypeConfigurations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "checksum")
            Lude.<*> (x Lude..?> "valueSelectionStrategy")
            Lude.<*> (x Lude..?> "createdDate")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "version")
            Lude.<*> (x Lude..?> "lastUpdatedDate")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (x Lude..?> "enumerationValues" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSlotType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetSlotType where
  toPath GetSlotType' {..} =
    Lude.mconcat
      ["/slottypes/", Lude.toBS name, "/versions/", Lude.toBS version]

instance Lude.ToQuery GetSlotType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSlotTypeResponse' smart constructor.
data GetSlotTypeResponse = GetSlotTypeResponse'
  { -- | The built-in slot type used as a parent for the slot type.
    parentSlotTypeSignature :: Lude.Maybe Lude.Text,
    -- | Configuration information that extends the parent built-in slot type.
    slotTypeConfigurations :: Lude.Maybe [SlotTypeConfiguration],
    -- | Checksum of the @> LATEST@ version of the slot type.
    checksum :: Lude.Maybe Lude.Text,
    -- | The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
    valueSelectionStrategy :: Lude.Maybe SlotValueSelectionStrategy,
    -- | The date that the slot type was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | The name of the slot type.
    name :: Lude.Maybe Lude.Text,
    -- | The version of the slot type.
    version :: Lude.Maybe Lude.Text,
    -- | The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    -- | A description of the slot type.
    description :: Lude.Maybe Lude.Text,
    -- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
    enumerationValues :: Lude.Maybe [EnumerationValue],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSlotTypeResponse' with the minimum fields required to make a request.
--
-- * 'parentSlotTypeSignature' - The built-in slot type used as a parent for the slot type.
-- * 'slotTypeConfigurations' - Configuration information that extends the parent built-in slot type.
-- * 'checksum' - Checksum of the @> LATEST@ version of the slot type.
-- * 'valueSelectionStrategy' - The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
-- * 'createdDate' - The date that the slot type was created.
-- * 'name' - The name of the slot type.
-- * 'version' - The version of the slot type.
-- * 'lastUpdatedDate' - The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
-- * 'description' - A description of the slot type.
-- * 'enumerationValues' - A list of @EnumerationValue@ objects that defines the values that the slot type can take.
-- * 'responseStatus' - The response status code.
mkGetSlotTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSlotTypeResponse
mkGetSlotTypeResponse pResponseStatus_ =
  GetSlotTypeResponse'
    { parentSlotTypeSignature = Lude.Nothing,
      slotTypeConfigurations = Lude.Nothing,
      checksum = Lude.Nothing,
      valueSelectionStrategy = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      lastUpdatedDate = Lude.Nothing,
      description = Lude.Nothing,
      enumerationValues = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The built-in slot type used as a parent for the slot type.
--
-- /Note:/ Consider using 'parentSlotTypeSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstfrsParentSlotTypeSignature :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe Lude.Text)
gstfrsParentSlotTypeSignature = Lens.lens (parentSlotTypeSignature :: GetSlotTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {parentSlotTypeSignature = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstfrsParentSlotTypeSignature "Use generic-lens or generic-optics with 'parentSlotTypeSignature' instead." #-}

-- | Configuration information that extends the parent built-in slot type.
--
-- /Note:/ Consider using 'slotTypeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstfrsSlotTypeConfigurations :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe [SlotTypeConfiguration])
gstfrsSlotTypeConfigurations = Lens.lens (slotTypeConfigurations :: GetSlotTypeResponse -> Lude.Maybe [SlotTypeConfiguration]) (\s a -> s {slotTypeConfigurations = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstfrsSlotTypeConfigurations "Use generic-lens or generic-optics with 'slotTypeConfigurations' instead." #-}

-- | Checksum of the @> LATEST@ version of the slot type.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstfrsChecksum :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe Lude.Text)
gstfrsChecksum = Lens.lens (checksum :: GetSlotTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstfrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
--
-- /Note:/ Consider using 'valueSelectionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstfrsValueSelectionStrategy :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe SlotValueSelectionStrategy)
gstfrsValueSelectionStrategy = Lens.lens (valueSelectionStrategy :: GetSlotTypeResponse -> Lude.Maybe SlotValueSelectionStrategy) (\s a -> s {valueSelectionStrategy = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstfrsValueSelectionStrategy "Use generic-lens or generic-optics with 'valueSelectionStrategy' instead." #-}

-- | The date that the slot type was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstfrsCreatedDate :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe Lude.Timestamp)
gstfrsCreatedDate = Lens.lens (createdDate :: GetSlotTypeResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstfrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the slot type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstfrsName :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe Lude.Text)
gstfrsName = Lens.lens (name :: GetSlotTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the slot type.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstfrsVersion :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe Lude.Text)
gstfrsVersion = Lens.lens (version :: GetSlotTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstfrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstfrsLastUpdatedDate :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe Lude.Timestamp)
gstfrsLastUpdatedDate = Lens.lens (lastUpdatedDate :: GetSlotTypeResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstfrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | A description of the slot type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstfrsDescription :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe Lude.Text)
gstfrsDescription = Lens.lens (description :: GetSlotTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstfrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
--
-- /Note:/ Consider using 'enumerationValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstfrsEnumerationValues :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe [EnumerationValue])
gstfrsEnumerationValues = Lens.lens (enumerationValues :: GetSlotTypeResponse -> Lude.Maybe [EnumerationValue]) (\s a -> s {enumerationValues = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstfrsEnumerationValues "Use generic-lens or generic-optics with 'enumerationValues' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstfrsResponseStatus :: Lens.Lens' GetSlotTypeResponse Lude.Int
gstfrsResponseStatus = Lens.lens (responseStatus :: GetSlotTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
