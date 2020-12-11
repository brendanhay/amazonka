{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gstsrsParentSlotTypeSignature,
    gstsrsSlotTypeConfigurations,
    gstsrsChecksum,
    gstsrsValueSelectionStrategy,
    gstsrsCreatedDate,
    gstsrsName,
    gstsrsVersion,
    gstsrsLastUpdatedDate,
    gstsrsDescription,
    gstsrsEnumerationValues,
    gstsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSlotType' smart constructor.
data GetSlotType = GetSlotType'
  { name :: Lude.Text,
    version :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
  { parentSlotTypeSignature ::
      Lude.Maybe Lude.Text,
    slotTypeConfigurations ::
      Lude.Maybe [SlotTypeConfiguration],
    checksum :: Lude.Maybe Lude.Text,
    valueSelectionStrategy ::
      Lude.Maybe SlotValueSelectionStrategy,
    createdDate :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    description :: Lude.Maybe Lude.Text,
    enumerationValues :: Lude.Maybe [EnumerationValue],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSlotTypeResponse' with the minimum fields required to make a request.
--
-- * 'checksum' - Checksum of the @> LATEST@ version of the slot type.
-- * 'createdDate' - The date that the slot type was created.
-- * 'description' - A description of the slot type.
-- * 'enumerationValues' - A list of @EnumerationValue@ objects that defines the values that the slot type can take.
-- * 'lastUpdatedDate' - The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
-- * 'name' - The name of the slot type.
-- * 'parentSlotTypeSignature' - The built-in slot type used as a parent for the slot type.
-- * 'responseStatus' - The response status code.
-- * 'slotTypeConfigurations' - Configuration information that extends the parent built-in slot type.
-- * 'valueSelectionStrategy' - The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
-- * 'version' - The version of the slot type.
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
gstsrsParentSlotTypeSignature :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe Lude.Text)
gstsrsParentSlotTypeSignature = Lens.lens (parentSlotTypeSignature :: GetSlotTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {parentSlotTypeSignature = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstsrsParentSlotTypeSignature "Use generic-lens or generic-optics with 'parentSlotTypeSignature' instead." #-}

-- | Configuration information that extends the parent built-in slot type.
--
-- /Note:/ Consider using 'slotTypeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstsrsSlotTypeConfigurations :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe [SlotTypeConfiguration])
gstsrsSlotTypeConfigurations = Lens.lens (slotTypeConfigurations :: GetSlotTypeResponse -> Lude.Maybe [SlotTypeConfiguration]) (\s a -> s {slotTypeConfigurations = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstsrsSlotTypeConfigurations "Use generic-lens or generic-optics with 'slotTypeConfigurations' instead." #-}

-- | Checksum of the @> LATEST@ version of the slot type.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstsrsChecksum :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe Lude.Text)
gstsrsChecksum = Lens.lens (checksum :: GetSlotTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstsrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
--
-- /Note:/ Consider using 'valueSelectionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstsrsValueSelectionStrategy :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe SlotValueSelectionStrategy)
gstsrsValueSelectionStrategy = Lens.lens (valueSelectionStrategy :: GetSlotTypeResponse -> Lude.Maybe SlotValueSelectionStrategy) (\s a -> s {valueSelectionStrategy = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstsrsValueSelectionStrategy "Use generic-lens or generic-optics with 'valueSelectionStrategy' instead." #-}

-- | The date that the slot type was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstsrsCreatedDate :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe Lude.Timestamp)
gstsrsCreatedDate = Lens.lens (createdDate :: GetSlotTypeResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstsrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the slot type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstsrsName :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe Lude.Text)
gstsrsName = Lens.lens (name :: GetSlotTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstsrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the slot type.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstsrsVersion :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe Lude.Text)
gstsrsVersion = Lens.lens (version :: GetSlotTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstsrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstsrsLastUpdatedDate :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe Lude.Timestamp)
gstsrsLastUpdatedDate = Lens.lens (lastUpdatedDate :: GetSlotTypeResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstsrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | A description of the slot type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstsrsDescription :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe Lude.Text)
gstsrsDescription = Lens.lens (description :: GetSlotTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstsrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
--
-- /Note:/ Consider using 'enumerationValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstsrsEnumerationValues :: Lens.Lens' GetSlotTypeResponse (Lude.Maybe [EnumerationValue])
gstsrsEnumerationValues = Lens.lens (enumerationValues :: GetSlotTypeResponse -> Lude.Maybe [EnumerationValue]) (\s a -> s {enumerationValues = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstsrsEnumerationValues "Use generic-lens or generic-optics with 'enumerationValues' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstsrsResponseStatus :: Lens.Lens' GetSlotTypeResponse Lude.Int
gstsrsResponseStatus = Lens.lens (responseStatus :: GetSlotTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSlotTypeResponse)
{-# DEPRECATED gstsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
