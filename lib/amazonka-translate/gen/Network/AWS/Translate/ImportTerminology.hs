{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.ImportTerminology
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a custom terminology, depending on whether or not one already exists for the given terminology name. Importing a terminology with the same name as an existing one will merge the terminologies based on the chosen merge strategy. Currently, the only supported merge strategy is OVERWRITE, and so the imported terminology will overwrite an existing terminology of the same name.
--
-- If you import a terminology that overwrites an existing one, the new terminology take up to 10 minutes to fully propagate and be available for use in a translation due to cache policies with the DataPlane service that performs the translations.
module Network.AWS.Translate.ImportTerminology
  ( -- * Creating a request
    ImportTerminology (..),
    mkImportTerminology,

    -- ** Request lenses
    itName,
    itEncryptionKey,
    itMergeStrategy,
    itTerminologyData,
    itDescription,

    -- * Destructuring the response
    ImportTerminologyResponse (..),
    mkImportTerminologyResponse,

    -- ** Response lenses
    itrsTerminologyProperties,
    itrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Translate.Types

-- | /See:/ 'mkImportTerminology' smart constructor.
data ImportTerminology = ImportTerminology'
  { -- | The name of the custom terminology being imported.
    name :: Lude.Text,
    -- | The encryption key for the custom terminology being imported.
    encryptionKey :: Lude.Maybe EncryptionKey,
    -- | The merge strategy of the custom terminology being imported. Currently, only the OVERWRITE merge strategy is supported. In this case, the imported terminology will overwrite an existing terminology of the same name.
    mergeStrategy :: MergeStrategy,
    -- | The terminology data for the custom terminology being imported.
    terminologyData :: TerminologyData,
    -- | The description of the custom terminology being imported.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportTerminology' with the minimum fields required to make a request.
--
-- * 'name' - The name of the custom terminology being imported.
-- * 'encryptionKey' - The encryption key for the custom terminology being imported.
-- * 'mergeStrategy' - The merge strategy of the custom terminology being imported. Currently, only the OVERWRITE merge strategy is supported. In this case, the imported terminology will overwrite an existing terminology of the same name.
-- * 'terminologyData' - The terminology data for the custom terminology being imported.
-- * 'description' - The description of the custom terminology being imported.
mkImportTerminology ::
  -- | 'name'
  Lude.Text ->
  -- | 'mergeStrategy'
  MergeStrategy ->
  -- | 'terminologyData'
  TerminologyData ->
  ImportTerminology
mkImportTerminology pName_ pMergeStrategy_ pTerminologyData_ =
  ImportTerminology'
    { name = pName_,
      encryptionKey = Lude.Nothing,
      mergeStrategy = pMergeStrategy_,
      terminologyData = pTerminologyData_,
      description = Lude.Nothing
    }

-- | The name of the custom terminology being imported.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itName :: Lens.Lens' ImportTerminology Lude.Text
itName = Lens.lens (name :: ImportTerminology -> Lude.Text) (\s a -> s {name = a} :: ImportTerminology)
{-# DEPRECATED itName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The encryption key for the custom terminology being imported.
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itEncryptionKey :: Lens.Lens' ImportTerminology (Lude.Maybe EncryptionKey)
itEncryptionKey = Lens.lens (encryptionKey :: ImportTerminology -> Lude.Maybe EncryptionKey) (\s a -> s {encryptionKey = a} :: ImportTerminology)
{-# DEPRECATED itEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

-- | The merge strategy of the custom terminology being imported. Currently, only the OVERWRITE merge strategy is supported. In this case, the imported terminology will overwrite an existing terminology of the same name.
--
-- /Note:/ Consider using 'mergeStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itMergeStrategy :: Lens.Lens' ImportTerminology MergeStrategy
itMergeStrategy = Lens.lens (mergeStrategy :: ImportTerminology -> MergeStrategy) (\s a -> s {mergeStrategy = a} :: ImportTerminology)
{-# DEPRECATED itMergeStrategy "Use generic-lens or generic-optics with 'mergeStrategy' instead." #-}

-- | The terminology data for the custom terminology being imported.
--
-- /Note:/ Consider using 'terminologyData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itTerminologyData :: Lens.Lens' ImportTerminology TerminologyData
itTerminologyData = Lens.lens (terminologyData :: ImportTerminology -> TerminologyData) (\s a -> s {terminologyData = a} :: ImportTerminology)
{-# DEPRECATED itTerminologyData "Use generic-lens or generic-optics with 'terminologyData' instead." #-}

-- | The description of the custom terminology being imported.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itDescription :: Lens.Lens' ImportTerminology (Lude.Maybe Lude.Text)
itDescription = Lens.lens (description :: ImportTerminology -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImportTerminology)
{-# DEPRECATED itDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest ImportTerminology where
  type Rs ImportTerminology = ImportTerminologyResponse
  request = Req.postJSON translateService
  response =
    Res.receiveJSON
      ( \s h x ->
          ImportTerminologyResponse'
            Lude.<$> (x Lude..?> "TerminologyProperties")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportTerminology where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShineFrontendService_20170701.ImportTerminology" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ImportTerminology where
  toJSON ImportTerminology' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            ("EncryptionKey" Lude..=) Lude.<$> encryptionKey,
            Lude.Just ("MergeStrategy" Lude..= mergeStrategy),
            Lude.Just ("TerminologyData" Lude..= terminologyData),
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath ImportTerminology where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportTerminology where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkImportTerminologyResponse' smart constructor.
data ImportTerminologyResponse = ImportTerminologyResponse'
  { -- | The properties of the custom terminology being imported.
    terminologyProperties :: Lude.Maybe TerminologyProperties,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportTerminologyResponse' with the minimum fields required to make a request.
--
-- * 'terminologyProperties' - The properties of the custom terminology being imported.
-- * 'responseStatus' - The response status code.
mkImportTerminologyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportTerminologyResponse
mkImportTerminologyResponse pResponseStatus_ =
  ImportTerminologyResponse'
    { terminologyProperties = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The properties of the custom terminology being imported.
--
-- /Note:/ Consider using 'terminologyProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itrsTerminologyProperties :: Lens.Lens' ImportTerminologyResponse (Lude.Maybe TerminologyProperties)
itrsTerminologyProperties = Lens.lens (terminologyProperties :: ImportTerminologyResponse -> Lude.Maybe TerminologyProperties) (\s a -> s {terminologyProperties = a} :: ImportTerminologyResponse)
{-# DEPRECATED itrsTerminologyProperties "Use generic-lens or generic-optics with 'terminologyProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itrsResponseStatus :: Lens.Lens' ImportTerminologyResponse Lude.Int
itrsResponseStatus = Lens.lens (responseStatus :: ImportTerminologyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportTerminologyResponse)
{-# DEPRECATED itrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
