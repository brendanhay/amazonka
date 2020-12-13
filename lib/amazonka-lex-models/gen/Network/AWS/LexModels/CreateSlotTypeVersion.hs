{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.CreateSlotTypeVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of a slot type based on the @> LATEST@ version of the specified slot type. If the @> LATEST@ version of this resource has not changed since the last version that you created, Amazon Lex doesn't create a new version. It returns the last version that you created.
--
-- When you create a version of a slot type, Amazon Lex sets the version to 1. Subsequent versions increment by 1. For more information, see 'versioning-intro' .
-- This operation requires permissions for the @lex:CreateSlotTypeVersion@ action.
module Network.AWS.LexModels.CreateSlotTypeVersion
  ( -- * Creating a request
    CreateSlotTypeVersion (..),
    mkCreateSlotTypeVersion,

    -- ** Request lenses
    cstvChecksum,
    cstvName,

    -- * Destructuring the response
    CreateSlotTypeVersionResponse (..),
    mkCreateSlotTypeVersionResponse,

    -- ** Response lenses
    cstvrsParentSlotTypeSignature,
    cstvrsSlotTypeConfigurations,
    cstvrsChecksum,
    cstvrsValueSelectionStrategy,
    cstvrsCreatedDate,
    cstvrsName,
    cstvrsVersion,
    cstvrsLastUpdatedDate,
    cstvrsDescription,
    cstvrsEnumerationValues,
    cstvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSlotTypeVersion' smart constructor.
data CreateSlotTypeVersion = CreateSlotTypeVersion'
  { -- | Checksum for the @> LATEST@ version of the slot type that you want to publish. If you specify a checksum and the @> LATEST@ version of the slot type has a different checksum, Amazon Lex returns a @PreconditionFailedException@ exception and doesn't publish the new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
    checksum :: Lude.Maybe Lude.Text,
    -- | The name of the slot type that you want to create a new version for. The name is case sensitive.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSlotTypeVersion' with the minimum fields required to make a request.
--
-- * 'checksum' - Checksum for the @> LATEST@ version of the slot type that you want to publish. If you specify a checksum and the @> LATEST@ version of the slot type has a different checksum, Amazon Lex returns a @PreconditionFailedException@ exception and doesn't publish the new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
-- * 'name' - The name of the slot type that you want to create a new version for. The name is case sensitive.
mkCreateSlotTypeVersion ::
  -- | 'name'
  Lude.Text ->
  CreateSlotTypeVersion
mkCreateSlotTypeVersion pName_ =
  CreateSlotTypeVersion' {checksum = Lude.Nothing, name = pName_}

-- | Checksum for the @> LATEST@ version of the slot type that you want to publish. If you specify a checksum and the @> LATEST@ version of the slot type has a different checksum, Amazon Lex returns a @PreconditionFailedException@ exception and doesn't publish the new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvChecksum :: Lens.Lens' CreateSlotTypeVersion (Lude.Maybe Lude.Text)
cstvChecksum = Lens.lens (checksum :: CreateSlotTypeVersion -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: CreateSlotTypeVersion)
{-# DEPRECATED cstvChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The name of the slot type that you want to create a new version for. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvName :: Lens.Lens' CreateSlotTypeVersion Lude.Text
cstvName = Lens.lens (name :: CreateSlotTypeVersion -> Lude.Text) (\s a -> s {name = a} :: CreateSlotTypeVersion)
{-# DEPRECATED cstvName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateSlotTypeVersion where
  type Rs CreateSlotTypeVersion = CreateSlotTypeVersionResponse
  request = Req.postJSON lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSlotTypeVersionResponse'
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

instance Lude.ToHeaders CreateSlotTypeVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSlotTypeVersion where
  toJSON CreateSlotTypeVersion' {..} =
    Lude.object
      (Lude.catMaybes [("checksum" Lude..=) Lude.<$> checksum])

instance Lude.ToPath CreateSlotTypeVersion where
  toPath CreateSlotTypeVersion' {..} =
    Lude.mconcat ["/slottypes/", Lude.toBS name, "/versions"]

instance Lude.ToQuery CreateSlotTypeVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSlotTypeVersionResponse' smart constructor.
data CreateSlotTypeVersionResponse = CreateSlotTypeVersionResponse'
  { -- | The built-in slot type used a the parent of the slot type.
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
    -- | The version assigned to the new slot type version.
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

-- | Creates a value of 'CreateSlotTypeVersionResponse' with the minimum fields required to make a request.
--
-- * 'parentSlotTypeSignature' - The built-in slot type used a the parent of the slot type.
-- * 'slotTypeConfigurations' - Configuration information that extends the parent built-in slot type.
-- * 'checksum' - Checksum of the @> LATEST@ version of the slot type.
-- * 'valueSelectionStrategy' - The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
-- * 'createdDate' - The date that the slot type was created.
-- * 'name' - The name of the slot type.
-- * 'version' - The version assigned to the new slot type version.
-- * 'lastUpdatedDate' - The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
-- * 'description' - A description of the slot type.
-- * 'enumerationValues' - A list of @EnumerationValue@ objects that defines the values that the slot type can take.
-- * 'responseStatus' - The response status code.
mkCreateSlotTypeVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSlotTypeVersionResponse
mkCreateSlotTypeVersionResponse pResponseStatus_ =
  CreateSlotTypeVersionResponse'
    { parentSlotTypeSignature =
        Lude.Nothing,
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

-- | The built-in slot type used a the parent of the slot type.
--
-- /Note:/ Consider using 'parentSlotTypeSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrsParentSlotTypeSignature :: Lens.Lens' CreateSlotTypeVersionResponse (Lude.Maybe Lude.Text)
cstvrsParentSlotTypeSignature = Lens.lens (parentSlotTypeSignature :: CreateSlotTypeVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {parentSlotTypeSignature = a} :: CreateSlotTypeVersionResponse)
{-# DEPRECATED cstvrsParentSlotTypeSignature "Use generic-lens or generic-optics with 'parentSlotTypeSignature' instead." #-}

-- | Configuration information that extends the parent built-in slot type.
--
-- /Note:/ Consider using 'slotTypeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrsSlotTypeConfigurations :: Lens.Lens' CreateSlotTypeVersionResponse (Lude.Maybe [SlotTypeConfiguration])
cstvrsSlotTypeConfigurations = Lens.lens (slotTypeConfigurations :: CreateSlotTypeVersionResponse -> Lude.Maybe [SlotTypeConfiguration]) (\s a -> s {slotTypeConfigurations = a} :: CreateSlotTypeVersionResponse)
{-# DEPRECATED cstvrsSlotTypeConfigurations "Use generic-lens or generic-optics with 'slotTypeConfigurations' instead." #-}

-- | Checksum of the @> LATEST@ version of the slot type.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrsChecksum :: Lens.Lens' CreateSlotTypeVersionResponse (Lude.Maybe Lude.Text)
cstvrsChecksum = Lens.lens (checksum :: CreateSlotTypeVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: CreateSlotTypeVersionResponse)
{-# DEPRECATED cstvrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
--
-- /Note:/ Consider using 'valueSelectionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrsValueSelectionStrategy :: Lens.Lens' CreateSlotTypeVersionResponse (Lude.Maybe SlotValueSelectionStrategy)
cstvrsValueSelectionStrategy = Lens.lens (valueSelectionStrategy :: CreateSlotTypeVersionResponse -> Lude.Maybe SlotValueSelectionStrategy) (\s a -> s {valueSelectionStrategy = a} :: CreateSlotTypeVersionResponse)
{-# DEPRECATED cstvrsValueSelectionStrategy "Use generic-lens or generic-optics with 'valueSelectionStrategy' instead." #-}

-- | The date that the slot type was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrsCreatedDate :: Lens.Lens' CreateSlotTypeVersionResponse (Lude.Maybe Lude.Timestamp)
cstvrsCreatedDate = Lens.lens (createdDate :: CreateSlotTypeVersionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: CreateSlotTypeVersionResponse)
{-# DEPRECATED cstvrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the slot type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrsName :: Lens.Lens' CreateSlotTypeVersionResponse (Lude.Maybe Lude.Text)
cstvrsName = Lens.lens (name :: CreateSlotTypeVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateSlotTypeVersionResponse)
{-# DEPRECATED cstvrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version assigned to the new slot type version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrsVersion :: Lens.Lens' CreateSlotTypeVersionResponse (Lude.Maybe Lude.Text)
cstvrsVersion = Lens.lens (version :: CreateSlotTypeVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: CreateSlotTypeVersionResponse)
{-# DEPRECATED cstvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrsLastUpdatedDate :: Lens.Lens' CreateSlotTypeVersionResponse (Lude.Maybe Lude.Timestamp)
cstvrsLastUpdatedDate = Lens.lens (lastUpdatedDate :: CreateSlotTypeVersionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: CreateSlotTypeVersionResponse)
{-# DEPRECATED cstvrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | A description of the slot type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrsDescription :: Lens.Lens' CreateSlotTypeVersionResponse (Lude.Maybe Lude.Text)
cstvrsDescription = Lens.lens (description :: CreateSlotTypeVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateSlotTypeVersionResponse)
{-# DEPRECATED cstvrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
--
-- /Note:/ Consider using 'enumerationValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrsEnumerationValues :: Lens.Lens' CreateSlotTypeVersionResponse (Lude.Maybe [EnumerationValue])
cstvrsEnumerationValues = Lens.lens (enumerationValues :: CreateSlotTypeVersionResponse -> Lude.Maybe [EnumerationValue]) (\s a -> s {enumerationValues = a} :: CreateSlotTypeVersionResponse)
{-# DEPRECATED cstvrsEnumerationValues "Use generic-lens or generic-optics with 'enumerationValues' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrsResponseStatus :: Lens.Lens' CreateSlotTypeVersionResponse Lude.Int
cstvrsResponseStatus = Lens.lens (responseStatus :: CreateSlotTypeVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSlotTypeVersionResponse)
{-# DEPRECATED cstvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
