{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GenerateChangeSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a target change set for a currently launched stack and writes it to an Amazon S3 object in the customer’s Amazon S3 bucket.
module Network.AWS.SMS.GenerateChangeSet
  ( -- * Creating a request
    GenerateChangeSet (..),
    mkGenerateChangeSet,

    -- ** Request lenses
    gcsAppId,
    gcsChangesetFormat,

    -- * Destructuring the response
    GenerateChangeSetResponse (..),
    mkGenerateChangeSetResponse,

    -- ** Response lenses
    gcsrsS3Location,
    gcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkGenerateChangeSet' smart constructor.
data GenerateChangeSet = GenerateChangeSet'
  { appId ::
      Lude.Maybe Lude.Text,
    changesetFormat :: Lude.Maybe OutputFormat
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateChangeSet' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application associated with the change set.
-- * 'changesetFormat' - The format for the change set.
mkGenerateChangeSet ::
  GenerateChangeSet
mkGenerateChangeSet =
  GenerateChangeSet'
    { appId = Lude.Nothing,
      changesetFormat = Lude.Nothing
    }

-- | The ID of the application associated with the change set.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsAppId :: Lens.Lens' GenerateChangeSet (Lude.Maybe Lude.Text)
gcsAppId = Lens.lens (appId :: GenerateChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: GenerateChangeSet)
{-# DEPRECATED gcsAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The format for the change set.
--
-- /Note:/ Consider using 'changesetFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsChangesetFormat :: Lens.Lens' GenerateChangeSet (Lude.Maybe OutputFormat)
gcsChangesetFormat = Lens.lens (changesetFormat :: GenerateChangeSet -> Lude.Maybe OutputFormat) (\s a -> s {changesetFormat = a} :: GenerateChangeSet)
{-# DEPRECATED gcsChangesetFormat "Use generic-lens or generic-optics with 'changesetFormat' instead." #-}

instance Lude.AWSRequest GenerateChangeSet where
  type Rs GenerateChangeSet = GenerateChangeSetResponse
  request = Req.postJSON smsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GenerateChangeSetResponse'
            Lude.<$> (x Lude..?> "s3Location") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GenerateChangeSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.GenerateChangeSet" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GenerateChangeSet where
  toJSON GenerateChangeSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("appId" Lude..=) Lude.<$> appId,
            ("changesetFormat" Lude..=) Lude.<$> changesetFormat
          ]
      )

instance Lude.ToPath GenerateChangeSet where
  toPath = Lude.const "/"

instance Lude.ToQuery GenerateChangeSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGenerateChangeSetResponse' smart constructor.
data GenerateChangeSetResponse = GenerateChangeSetResponse'
  { s3Location ::
      Lude.Maybe S3Location,
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

-- | Creates a value of 'GenerateChangeSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 's3Location' - The location of the Amazon S3 object.
mkGenerateChangeSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GenerateChangeSetResponse
mkGenerateChangeSetResponse pResponseStatus_ =
  GenerateChangeSetResponse'
    { s3Location = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The location of the Amazon S3 object.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsS3Location :: Lens.Lens' GenerateChangeSetResponse (Lude.Maybe S3Location)
gcsrsS3Location = Lens.lens (s3Location :: GenerateChangeSetResponse -> Lude.Maybe S3Location) (\s a -> s {s3Location = a} :: GenerateChangeSetResponse)
{-# DEPRECATED gcsrsS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsResponseStatus :: Lens.Lens' GenerateChangeSetResponse Lude.Int
gcsrsResponseStatus = Lens.lens (responseStatus :: GenerateChangeSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GenerateChangeSetResponse)
{-# DEPRECATED gcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
