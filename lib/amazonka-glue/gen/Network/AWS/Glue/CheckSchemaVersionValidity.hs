{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    csvvrsError,
    csvvrsValid,
    csvvrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCheckSchemaVersionValidity' smart constructor.
data CheckSchemaVersionValidity = CheckSchemaVersionValidity'
  { dataFormat ::
      DataFormat,
    schemaDefinition :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CheckSchemaVersionValidity' with the minimum fields required to make a request.
--
-- * 'dataFormat' - The data format of the schema definition. Currently only @AVRO@ is supported.
-- * 'schemaDefinition' - The definition of the schema that has to be validated.
mkCheckSchemaVersionValidity ::
  -- | 'dataFormat'
  DataFormat ->
  -- | 'schemaDefinition'
  Lude.Text ->
  CheckSchemaVersionValidity
mkCheckSchemaVersionValidity pDataFormat_ pSchemaDefinition_ =
  CheckSchemaVersionValidity'
    { dataFormat = pDataFormat_,
      schemaDefinition = pSchemaDefinition_
    }

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvvDataFormat :: Lens.Lens' CheckSchemaVersionValidity DataFormat
csvvDataFormat = Lens.lens (dataFormat :: CheckSchemaVersionValidity -> DataFormat) (\s a -> s {dataFormat = a} :: CheckSchemaVersionValidity)
{-# DEPRECATED csvvDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

-- | The definition of the schema that has to be validated.
--
-- /Note:/ Consider using 'schemaDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvvSchemaDefinition :: Lens.Lens' CheckSchemaVersionValidity Lude.Text
csvvSchemaDefinition = Lens.lens (schemaDefinition :: CheckSchemaVersionValidity -> Lude.Text) (\s a -> s {schemaDefinition = a} :: CheckSchemaVersionValidity)
{-# DEPRECATED csvvSchemaDefinition "Use generic-lens or generic-optics with 'schemaDefinition' instead." #-}

instance Lude.AWSRequest CheckSchemaVersionValidity where
  type
    Rs CheckSchemaVersionValidity =
      CheckSchemaVersionValidityResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          CheckSchemaVersionValidityResponse'
            Lude.<$> (x Lude..?> "Error")
            Lude.<*> (x Lude..?> "Valid")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CheckSchemaVersionValidity where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CheckSchemaVersionValidity" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CheckSchemaVersionValidity where
  toJSON CheckSchemaVersionValidity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DataFormat" Lude..= dataFormat),
            Lude.Just ("SchemaDefinition" Lude..= schemaDefinition)
          ]
      )

instance Lude.ToPath CheckSchemaVersionValidity where
  toPath = Lude.const "/"

instance Lude.ToQuery CheckSchemaVersionValidity where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCheckSchemaVersionValidityResponse' smart constructor.
data CheckSchemaVersionValidityResponse = CheckSchemaVersionValidityResponse'
  { error ::
      Lude.Maybe Lude.Text,
    valid ::
      Lude.Maybe Lude.Bool,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CheckSchemaVersionValidityResponse' with the minimum fields required to make a request.
--
-- * 'error' - A validation failure error message.
-- * 'responseStatus' - The response status code.
-- * 'valid' - Return true, if the schema is valid and false otherwise.
mkCheckSchemaVersionValidityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CheckSchemaVersionValidityResponse
mkCheckSchemaVersionValidityResponse pResponseStatus_ =
  CheckSchemaVersionValidityResponse'
    { error = Lude.Nothing,
      valid = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A validation failure error message.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvvrsError :: Lens.Lens' CheckSchemaVersionValidityResponse (Lude.Maybe Lude.Text)
csvvrsError = Lens.lens (error :: CheckSchemaVersionValidityResponse -> Lude.Maybe Lude.Text) (\s a -> s {error = a} :: CheckSchemaVersionValidityResponse)
{-# DEPRECATED csvvrsError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | Return true, if the schema is valid and false otherwise.
--
-- /Note:/ Consider using 'valid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvvrsValid :: Lens.Lens' CheckSchemaVersionValidityResponse (Lude.Maybe Lude.Bool)
csvvrsValid = Lens.lens (valid :: CheckSchemaVersionValidityResponse -> Lude.Maybe Lude.Bool) (\s a -> s {valid = a} :: CheckSchemaVersionValidityResponse)
{-# DEPRECATED csvvrsValid "Use generic-lens or generic-optics with 'valid' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvvrsResponseStatus :: Lens.Lens' CheckSchemaVersionValidityResponse Lude.Int
csvvrsResponseStatus = Lens.lens (responseStatus :: CheckSchemaVersionValidityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CheckSchemaVersionValidityResponse)
{-# DEPRECATED csvvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
