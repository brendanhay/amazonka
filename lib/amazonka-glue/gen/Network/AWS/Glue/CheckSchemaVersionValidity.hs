{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CheckSchemaVersionValidity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the supplied schema. This call has no side effects, it simply validates using the supplied schema using @DataFormat@ as the format. Since it does not take a schema set name, no compatibility checks are performed.
module Network.AWS.Glue.CheckSchemaVersionValidity
  ( -- * Creating a request
    CheckSchemaVersionValidity (..),
    mkCheckSchemaVersionValidity,

    -- ** Request lenses
    csvvDataFormat,
    csvvSchemaDefinition,

    -- * Destructuring the response
    CheckSchemaVersionValidityResponse (..),
    mkCheckSchemaVersionValidityResponse,

    -- ** Response lenses
    csvvrrsError,
    csvvrrsValid,
    csvvrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCheckSchemaVersionValidity' smart constructor.
data CheckSchemaVersionValidity = CheckSchemaVersionValidity'
  { -- | The data format of the schema definition. Currently only @AVRO@ is supported.
    dataFormat :: Types.DataFormat,
    -- | The definition of the schema that has to be validated.
    schemaDefinition :: Types.SchemaDefinitionString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CheckSchemaVersionValidity' value with any optional fields omitted.
mkCheckSchemaVersionValidity ::
  -- | 'dataFormat'
  Types.DataFormat ->
  -- | 'schemaDefinition'
  Types.SchemaDefinitionString ->
  CheckSchemaVersionValidity
mkCheckSchemaVersionValidity dataFormat schemaDefinition =
  CheckSchemaVersionValidity' {dataFormat, schemaDefinition}

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvvDataFormat :: Lens.Lens' CheckSchemaVersionValidity Types.DataFormat
csvvDataFormat = Lens.field @"dataFormat"
{-# DEPRECATED csvvDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

-- | The definition of the schema that has to be validated.
--
-- /Note:/ Consider using 'schemaDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvvSchemaDefinition :: Lens.Lens' CheckSchemaVersionValidity Types.SchemaDefinitionString
csvvSchemaDefinition = Lens.field @"schemaDefinition"
{-# DEPRECATED csvvSchemaDefinition "Use generic-lens or generic-optics with 'schemaDefinition' instead." #-}

instance Core.FromJSON CheckSchemaVersionValidity where
  toJSON CheckSchemaVersionValidity {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DataFormat" Core..= dataFormat),
            Core.Just ("SchemaDefinition" Core..= schemaDefinition)
          ]
      )

instance Core.AWSRequest CheckSchemaVersionValidity where
  type
    Rs CheckSchemaVersionValidity =
      CheckSchemaVersionValidityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.CheckSchemaVersionValidity")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CheckSchemaVersionValidityResponse'
            Core.<$> (x Core..:? "Error")
            Core.<*> (x Core..:? "Valid")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCheckSchemaVersionValidityResponse' smart constructor.
data CheckSchemaVersionValidityResponse = CheckSchemaVersionValidityResponse'
  { -- | A validation failure error message.
    error :: Core.Maybe Types.Error,
    -- | Return true, if the schema is valid and false otherwise.
    valid :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CheckSchemaVersionValidityResponse' value with any optional fields omitted.
mkCheckSchemaVersionValidityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CheckSchemaVersionValidityResponse
mkCheckSchemaVersionValidityResponse responseStatus =
  CheckSchemaVersionValidityResponse'
    { error = Core.Nothing,
      valid = Core.Nothing,
      responseStatus
    }

-- | A validation failure error message.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvvrrsError :: Lens.Lens' CheckSchemaVersionValidityResponse (Core.Maybe Types.Error)
csvvrrsError = Lens.field @"error"
{-# DEPRECATED csvvrrsError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | Return true, if the schema is valid and false otherwise.
--
-- /Note:/ Consider using 'valid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvvrrsValid :: Lens.Lens' CheckSchemaVersionValidityResponse (Core.Maybe Core.Bool)
csvvrrsValid = Lens.field @"valid"
{-# DEPRECATED csvvrrsValid "Use generic-lens or generic-optics with 'valid' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvvrrsResponseStatus :: Lens.Lens' CheckSchemaVersionValidityResponse Core.Int
csvvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csvvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
