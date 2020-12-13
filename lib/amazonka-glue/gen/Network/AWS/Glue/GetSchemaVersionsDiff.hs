{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetSchemaVersionsDiff
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches the schema version difference in the specified difference type between two stored schema versions in the Schema Registry.
--
-- This API allows you to compare two schema versions between two schema definitions under the same schema.
module Network.AWS.Glue.GetSchemaVersionsDiff
  ( -- * Creating a request
    GetSchemaVersionsDiff (..),
    mkGetSchemaVersionsDiff,

    -- ** Request lenses
    gsvdSchemaId,
    gsvdSchemaDiffType,
    gsvdFirstSchemaVersionNumber,
    gsvdSecondSchemaVersionNumber,

    -- * Destructuring the response
    GetSchemaVersionsDiffResponse (..),
    mkGetSchemaVersionsDiffResponse,

    -- ** Response lenses
    gsvdrsDiff,
    gsvdrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSchemaVersionsDiff' smart constructor.
data GetSchemaVersionsDiff = GetSchemaVersionsDiff'
  { -- | This is a wrapper structure to contain schema identity fields. The structure contains:
    --
    --
    --     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
    --
    --
    --     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
    schemaId :: SchemaId,
    -- | Refers to @SYNTAX_DIFF@ , which is the currently supported diff type.
    schemaDiffType :: SchemaDiffType,
    -- | The first of the two schema versions to be compared.
    firstSchemaVersionNumber :: SchemaVersionNumber,
    -- | The second of the two schema versions to be compared.
    secondSchemaVersionNumber :: SchemaVersionNumber
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSchemaVersionsDiff' with the minimum fields required to make a request.
--
-- * 'schemaId' - This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
--
-- * 'schemaDiffType' - Refers to @SYNTAX_DIFF@ , which is the currently supported diff type.
-- * 'firstSchemaVersionNumber' - The first of the two schema versions to be compared.
-- * 'secondSchemaVersionNumber' - The second of the two schema versions to be compared.
mkGetSchemaVersionsDiff ::
  -- | 'schemaId'
  SchemaId ->
  -- | 'schemaDiffType'
  SchemaDiffType ->
  -- | 'firstSchemaVersionNumber'
  SchemaVersionNumber ->
  -- | 'secondSchemaVersionNumber'
  SchemaVersionNumber ->
  GetSchemaVersionsDiff
mkGetSchemaVersionsDiff
  pSchemaId_
  pSchemaDiffType_
  pFirstSchemaVersionNumber_
  pSecondSchemaVersionNumber_ =
    GetSchemaVersionsDiff'
      { schemaId = pSchemaId_,
        schemaDiffType = pSchemaDiffType_,
        firstSchemaVersionNumber = pFirstSchemaVersionNumber_,
        secondSchemaVersionNumber = pSecondSchemaVersionNumber_
      }

-- | This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
--
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvdSchemaId :: Lens.Lens' GetSchemaVersionsDiff SchemaId
gsvdSchemaId = Lens.lens (schemaId :: GetSchemaVersionsDiff -> SchemaId) (\s a -> s {schemaId = a} :: GetSchemaVersionsDiff)
{-# DEPRECATED gsvdSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

-- | Refers to @SYNTAX_DIFF@ , which is the currently supported diff type.
--
-- /Note:/ Consider using 'schemaDiffType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvdSchemaDiffType :: Lens.Lens' GetSchemaVersionsDiff SchemaDiffType
gsvdSchemaDiffType = Lens.lens (schemaDiffType :: GetSchemaVersionsDiff -> SchemaDiffType) (\s a -> s {schemaDiffType = a} :: GetSchemaVersionsDiff)
{-# DEPRECATED gsvdSchemaDiffType "Use generic-lens or generic-optics with 'schemaDiffType' instead." #-}

-- | The first of the two schema versions to be compared.
--
-- /Note:/ Consider using 'firstSchemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvdFirstSchemaVersionNumber :: Lens.Lens' GetSchemaVersionsDiff SchemaVersionNumber
gsvdFirstSchemaVersionNumber = Lens.lens (firstSchemaVersionNumber :: GetSchemaVersionsDiff -> SchemaVersionNumber) (\s a -> s {firstSchemaVersionNumber = a} :: GetSchemaVersionsDiff)
{-# DEPRECATED gsvdFirstSchemaVersionNumber "Use generic-lens or generic-optics with 'firstSchemaVersionNumber' instead." #-}

-- | The second of the two schema versions to be compared.
--
-- /Note:/ Consider using 'secondSchemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvdSecondSchemaVersionNumber :: Lens.Lens' GetSchemaVersionsDiff SchemaVersionNumber
gsvdSecondSchemaVersionNumber = Lens.lens (secondSchemaVersionNumber :: GetSchemaVersionsDiff -> SchemaVersionNumber) (\s a -> s {secondSchemaVersionNumber = a} :: GetSchemaVersionsDiff)
{-# DEPRECATED gsvdSecondSchemaVersionNumber "Use generic-lens or generic-optics with 'secondSchemaVersionNumber' instead." #-}

instance Lude.AWSRequest GetSchemaVersionsDiff where
  type Rs GetSchemaVersionsDiff = GetSchemaVersionsDiffResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSchemaVersionsDiffResponse'
            Lude.<$> (x Lude..?> "Diff") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSchemaVersionsDiff where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetSchemaVersionsDiff" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSchemaVersionsDiff where
  toJSON GetSchemaVersionsDiff' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SchemaId" Lude..= schemaId),
            Lude.Just ("SchemaDiffType" Lude..= schemaDiffType),
            Lude.Just
              ("FirstSchemaVersionNumber" Lude..= firstSchemaVersionNumber),
            Lude.Just
              ("SecondSchemaVersionNumber" Lude..= secondSchemaVersionNumber)
          ]
      )

instance Lude.ToPath GetSchemaVersionsDiff where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSchemaVersionsDiff where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSchemaVersionsDiffResponse' smart constructor.
data GetSchemaVersionsDiffResponse = GetSchemaVersionsDiffResponse'
  { -- | The difference between schemas as a string in JsonPatch format.
    diff :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSchemaVersionsDiffResponse' with the minimum fields required to make a request.
--
-- * 'diff' - The difference between schemas as a string in JsonPatch format.
-- * 'responseStatus' - The response status code.
mkGetSchemaVersionsDiffResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSchemaVersionsDiffResponse
mkGetSchemaVersionsDiffResponse pResponseStatus_ =
  GetSchemaVersionsDiffResponse'
    { diff = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The difference between schemas as a string in JsonPatch format.
--
-- /Note:/ Consider using 'diff' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvdrsDiff :: Lens.Lens' GetSchemaVersionsDiffResponse (Lude.Maybe Lude.Text)
gsvdrsDiff = Lens.lens (diff :: GetSchemaVersionsDiffResponse -> Lude.Maybe Lude.Text) (\s a -> s {diff = a} :: GetSchemaVersionsDiffResponse)
{-# DEPRECATED gsvdrsDiff "Use generic-lens or generic-optics with 'diff' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvdrsResponseStatus :: Lens.Lens' GetSchemaVersionsDiffResponse Lude.Int
gsvdrsResponseStatus = Lens.lens (responseStatus :: GetSchemaVersionsDiffResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSchemaVersionsDiffResponse)
{-# DEPRECATED gsvdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
