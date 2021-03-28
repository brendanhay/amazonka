{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ImportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.ImportTask
  ( ImportTask (..)
  -- * Smart constructor
  , mkImportTask
  -- * Lenses
  , itApplicationImportFailure
  , itApplicationImportSuccess
  , itClientRequestToken
  , itErrorsAndFailedEntriesZip
  , itImportCompletionTime
  , itImportDeletedTime
  , itImportRequestTime
  , itImportTaskId
  , itImportUrl
  , itName
  , itServerImportFailure
  , itServerImportSuccess
  , itStatus
  ) where

import qualified Network.AWS.Discovery.Types.ClientRequestToken as Types
import qualified Network.AWS.Discovery.Types.ErrorsAndFailedEntriesZip as Types
import qualified Network.AWS.Discovery.Types.ImportStatus as Types
import qualified Network.AWS.Discovery.Types.ImportTaskId as Types
import qualified Network.AWS.Discovery.Types.ImportUrl as Types
import qualified Network.AWS.Discovery.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An array of information related to the import task request that includes status information, times, IDs, the Amazon S3 Object URL for the import file, and more.
--
-- /See:/ 'mkImportTask' smart constructor.
data ImportTask = ImportTask'
  { applicationImportFailure :: Core.Maybe Core.Int
    -- ^ The total number of application records in the import file that failed to be imported.
  , applicationImportSuccess :: Core.Maybe Core.Int
    -- ^ The total number of application records in the import file that were successfully imported.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A unique token used to prevent the same import request from occurring more than once. If you didn't provide a token, a token was automatically generated when the import task request was sent.
  , errorsAndFailedEntriesZip :: Core.Maybe Types.ErrorsAndFailedEntriesZip
    -- ^ A link to a compressed archive folder (in the ZIP format) that contains an error log and a file of failed records. You can use these two files to quickly identify records that failed, why they failed, and correct those records. Afterward, you can upload the corrected file to your Amazon S3 bucket and create another import task request.
--
-- This field also includes authorization information so you can confirm the authenticity of the compressed archive before you download it.
-- If some records failed to be imported we recommend that you correct the records in the failed entries file and then imports that failed entries file. This prevents you from having to correct and update the larger original file and attempt importing it again.
  , importCompletionTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the import task request finished, presented in the Unix time stamp format.
  , importDeletedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the import task request was deleted, presented in the Unix time stamp format.
  , importRequestTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the import task request was made, presented in the Unix time stamp format.
  , importTaskId :: Core.Maybe Types.ImportTaskId
    -- ^ The unique ID for a specific import task. These IDs aren't globally unique, but they are unique within an AWS account.
  , importUrl :: Core.Maybe Types.ImportUrl
    -- ^ The URL for your import file that you've uploaded to Amazon S3.
  , name :: Core.Maybe Types.Name
    -- ^ A descriptive name for an import task. You can use this name to filter future requests related to this import task, such as identifying applications and servers that were included in this import task. We recommend that you use a meaningful name for each import task.
  , serverImportFailure :: Core.Maybe Core.Int
    -- ^ The total number of server records in the import file that failed to be imported.
  , serverImportSuccess :: Core.Maybe Core.Int
    -- ^ The total number of server records in the import file that were successfully imported.
  , status :: Core.Maybe Types.ImportStatus
    -- ^ The status of the import task. An import can have the status of @IMPORT_COMPLETE@ and still have some records fail to import from the overall request. More information can be found in the downloadable archive defined in the @errorsAndFailedEntriesZip@ field, or in the Migration Hub management console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ImportTask' value with any optional fields omitted.
mkImportTask
    :: ImportTask
mkImportTask
  = ImportTask'{applicationImportFailure = Core.Nothing,
                applicationImportSuccess = Core.Nothing,
                clientRequestToken = Core.Nothing,
                errorsAndFailedEntriesZip = Core.Nothing,
                importCompletionTime = Core.Nothing,
                importDeletedTime = Core.Nothing, importRequestTime = Core.Nothing,
                importTaskId = Core.Nothing, importUrl = Core.Nothing,
                name = Core.Nothing, serverImportFailure = Core.Nothing,
                serverImportSuccess = Core.Nothing, status = Core.Nothing}

-- | The total number of application records in the import file that failed to be imported.
--
-- /Note:/ Consider using 'applicationImportFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itApplicationImportFailure :: Lens.Lens' ImportTask (Core.Maybe Core.Int)
itApplicationImportFailure = Lens.field @"applicationImportFailure"
{-# INLINEABLE itApplicationImportFailure #-}
{-# DEPRECATED applicationImportFailure "Use generic-lens or generic-optics with 'applicationImportFailure' instead"  #-}

-- | The total number of application records in the import file that were successfully imported.
--
-- /Note:/ Consider using 'applicationImportSuccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itApplicationImportSuccess :: Lens.Lens' ImportTask (Core.Maybe Core.Int)
itApplicationImportSuccess = Lens.field @"applicationImportSuccess"
{-# INLINEABLE itApplicationImportSuccess #-}
{-# DEPRECATED applicationImportSuccess "Use generic-lens or generic-optics with 'applicationImportSuccess' instead"  #-}

-- | A unique token used to prevent the same import request from occurring more than once. If you didn't provide a token, a token was automatically generated when the import task request was sent.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itClientRequestToken :: Lens.Lens' ImportTask (Core.Maybe Types.ClientRequestToken)
itClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE itClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | A link to a compressed archive folder (in the ZIP format) that contains an error log and a file of failed records. You can use these two files to quickly identify records that failed, why they failed, and correct those records. Afterward, you can upload the corrected file to your Amazon S3 bucket and create another import task request.
--
-- This field also includes authorization information so you can confirm the authenticity of the compressed archive before you download it.
-- If some records failed to be imported we recommend that you correct the records in the failed entries file and then imports that failed entries file. This prevents you from having to correct and update the larger original file and attempt importing it again.
--
-- /Note:/ Consider using 'errorsAndFailedEntriesZip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itErrorsAndFailedEntriesZip :: Lens.Lens' ImportTask (Core.Maybe Types.ErrorsAndFailedEntriesZip)
itErrorsAndFailedEntriesZip = Lens.field @"errorsAndFailedEntriesZip"
{-# INLINEABLE itErrorsAndFailedEntriesZip #-}
{-# DEPRECATED errorsAndFailedEntriesZip "Use generic-lens or generic-optics with 'errorsAndFailedEntriesZip' instead"  #-}

-- | The time that the import task request finished, presented in the Unix time stamp format.
--
-- /Note:/ Consider using 'importCompletionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itImportCompletionTime :: Lens.Lens' ImportTask (Core.Maybe Core.NominalDiffTime)
itImportCompletionTime = Lens.field @"importCompletionTime"
{-# INLINEABLE itImportCompletionTime #-}
{-# DEPRECATED importCompletionTime "Use generic-lens or generic-optics with 'importCompletionTime' instead"  #-}

-- | The time that the import task request was deleted, presented in the Unix time stamp format.
--
-- /Note:/ Consider using 'importDeletedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itImportDeletedTime :: Lens.Lens' ImportTask (Core.Maybe Core.NominalDiffTime)
itImportDeletedTime = Lens.field @"importDeletedTime"
{-# INLINEABLE itImportDeletedTime #-}
{-# DEPRECATED importDeletedTime "Use generic-lens or generic-optics with 'importDeletedTime' instead"  #-}

-- | The time that the import task request was made, presented in the Unix time stamp format.
--
-- /Note:/ Consider using 'importRequestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itImportRequestTime :: Lens.Lens' ImportTask (Core.Maybe Core.NominalDiffTime)
itImportRequestTime = Lens.field @"importRequestTime"
{-# INLINEABLE itImportRequestTime #-}
{-# DEPRECATED importRequestTime "Use generic-lens or generic-optics with 'importRequestTime' instead"  #-}

-- | The unique ID for a specific import task. These IDs aren't globally unique, but they are unique within an AWS account.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itImportTaskId :: Lens.Lens' ImportTask (Core.Maybe Types.ImportTaskId)
itImportTaskId = Lens.field @"importTaskId"
{-# INLINEABLE itImportTaskId #-}
{-# DEPRECATED importTaskId "Use generic-lens or generic-optics with 'importTaskId' instead"  #-}

-- | The URL for your import file that you've uploaded to Amazon S3.
--
-- /Note:/ Consider using 'importUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itImportUrl :: Lens.Lens' ImportTask (Core.Maybe Types.ImportUrl)
itImportUrl = Lens.field @"importUrl"
{-# INLINEABLE itImportUrl #-}
{-# DEPRECATED importUrl "Use generic-lens or generic-optics with 'importUrl' instead"  #-}

-- | A descriptive name for an import task. You can use this name to filter future requests related to this import task, such as identifying applications and servers that were included in this import task. We recommend that you use a meaningful name for each import task.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itName :: Lens.Lens' ImportTask (Core.Maybe Types.Name)
itName = Lens.field @"name"
{-# INLINEABLE itName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The total number of server records in the import file that failed to be imported.
--
-- /Note:/ Consider using 'serverImportFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itServerImportFailure :: Lens.Lens' ImportTask (Core.Maybe Core.Int)
itServerImportFailure = Lens.field @"serverImportFailure"
{-# INLINEABLE itServerImportFailure #-}
{-# DEPRECATED serverImportFailure "Use generic-lens or generic-optics with 'serverImportFailure' instead"  #-}

-- | The total number of server records in the import file that were successfully imported.
--
-- /Note:/ Consider using 'serverImportSuccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itServerImportSuccess :: Lens.Lens' ImportTask (Core.Maybe Core.Int)
itServerImportSuccess = Lens.field @"serverImportSuccess"
{-# INLINEABLE itServerImportSuccess #-}
{-# DEPRECATED serverImportSuccess "Use generic-lens or generic-optics with 'serverImportSuccess' instead"  #-}

-- | The status of the import task. An import can have the status of @IMPORT_COMPLETE@ and still have some records fail to import from the overall request. More information can be found in the downloadable archive defined in the @errorsAndFailedEntriesZip@ field, or in the Migration Hub management console.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itStatus :: Lens.Lens' ImportTask (Core.Maybe Types.ImportStatus)
itStatus = Lens.field @"status"
{-# INLINEABLE itStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON ImportTask where
        parseJSON
          = Core.withObject "ImportTask" Core.$
              \ x ->
                ImportTask' Core.<$>
                  (x Core..:? "applicationImportFailure") Core.<*>
                    x Core..:? "applicationImportSuccess"
                    Core.<*> x Core..:? "clientRequestToken"
                    Core.<*> x Core..:? "errorsAndFailedEntriesZip"
                    Core.<*> x Core..:? "importCompletionTime"
                    Core.<*> x Core..:? "importDeletedTime"
                    Core.<*> x Core..:? "importRequestTime"
                    Core.<*> x Core..:? "importTaskId"
                    Core.<*> x Core..:? "importUrl"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "serverImportFailure"
                    Core.<*> x Core..:? "serverImportSuccess"
                    Core.<*> x Core..:? "status"
