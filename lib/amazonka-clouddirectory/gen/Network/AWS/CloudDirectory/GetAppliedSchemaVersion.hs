{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.GetAppliedSchemaVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns current applied schema version ARN, including the minor version in use.
module Network.AWS.CloudDirectory.GetAppliedSchemaVersion
  ( -- * Creating a request
    GetAppliedSchemaVersion (..),
    mkGetAppliedSchemaVersion,

    -- ** Request lenses
    gasvSchemaARN,

    -- * Destructuring the response
    GetAppliedSchemaVersionResponse (..),
    mkGetAppliedSchemaVersionResponse,

    -- ** Response lenses
    gasvrsAppliedSchemaARN,
    gasvrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAppliedSchemaVersion' smart constructor.
newtype GetAppliedSchemaVersion = GetAppliedSchemaVersion'
  { schemaARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAppliedSchemaVersion' with the minimum fields required to make a request.
--
-- * 'schemaARN' - The ARN of the applied schema.
mkGetAppliedSchemaVersion ::
  -- | 'schemaARN'
  Lude.Text ->
  GetAppliedSchemaVersion
mkGetAppliedSchemaVersion pSchemaARN_ =
  GetAppliedSchemaVersion' {schemaARN = pSchemaARN_}

-- | The ARN of the applied schema.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasvSchemaARN :: Lens.Lens' GetAppliedSchemaVersion Lude.Text
gasvSchemaARN = Lens.lens (schemaARN :: GetAppliedSchemaVersion -> Lude.Text) (\s a -> s {schemaARN = a} :: GetAppliedSchemaVersion)
{-# DEPRECATED gasvSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

instance Lude.AWSRequest GetAppliedSchemaVersion where
  type Rs GetAppliedSchemaVersion = GetAppliedSchemaVersionResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAppliedSchemaVersionResponse'
            Lude.<$> (x Lude..?> "AppliedSchemaArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAppliedSchemaVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetAppliedSchemaVersion where
  toJSON GetAppliedSchemaVersion' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SchemaArn" Lude..= schemaARN)])

instance Lude.ToPath GetAppliedSchemaVersion where
  toPath =
    Lude.const
      "/amazonclouddirectory/2017-01-11/schema/getappliedschema"

instance Lude.ToQuery GetAppliedSchemaVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAppliedSchemaVersionResponse' smart constructor.
data GetAppliedSchemaVersionResponse = GetAppliedSchemaVersionResponse'
  { appliedSchemaARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetAppliedSchemaVersionResponse' with the minimum fields required to make a request.
--
-- * 'appliedSchemaARN' - Current applied schema ARN, including the minor version in use if one was provided.
-- * 'responseStatus' - The response status code.
mkGetAppliedSchemaVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAppliedSchemaVersionResponse
mkGetAppliedSchemaVersionResponse pResponseStatus_ =
  GetAppliedSchemaVersionResponse'
    { appliedSchemaARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Current applied schema ARN, including the minor version in use if one was provided.
--
-- /Note:/ Consider using 'appliedSchemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasvrsAppliedSchemaARN :: Lens.Lens' GetAppliedSchemaVersionResponse (Lude.Maybe Lude.Text)
gasvrsAppliedSchemaARN = Lens.lens (appliedSchemaARN :: GetAppliedSchemaVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {appliedSchemaARN = a} :: GetAppliedSchemaVersionResponse)
{-# DEPRECATED gasvrsAppliedSchemaARN "Use generic-lens or generic-optics with 'appliedSchemaARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasvrsResponseStatus :: Lens.Lens' GetAppliedSchemaVersionResponse Lude.Int
gasvrsResponseStatus = Lens.lens (responseStatus :: GetAppliedSchemaVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAppliedSchemaVersionResponse)
{-# DEPRECATED gasvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
