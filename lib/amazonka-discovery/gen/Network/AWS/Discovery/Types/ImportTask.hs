-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ImportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ImportTask
  ( ImportTask (..),

    -- * Smart constructor
    mkImportTask,

    -- * Lenses
    itApplicationImportSuccess,
    itStatus,
    itServerImportSuccess,
    itImportCompletionTime,
    itName,
    itApplicationImportFailure,
    itErrorsAndFailedEntriesZip,
    itImportTaskId,
    itImportDeletedTime,
    itServerImportFailure,
    itClientRequestToken,
    itImportURL,
    itImportRequestTime,
  )
where

import Network.AWS.Discovery.Types.ImportStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An array of information related to the import task request that includes status information, times, IDs, the Amazon S3 Object URL for the import file, and more.
--
-- /See:/ 'mkImportTask' smart constructor.
data ImportTask = ImportTask'
  { applicationImportSuccess ::
      Lude.Maybe Lude.Int,
    status :: Lude.Maybe ImportStatus,
    serverImportSuccess :: Lude.Maybe Lude.Int,
    importCompletionTime :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    applicationImportFailure :: Lude.Maybe Lude.Int,
    errorsAndFailedEntriesZip :: Lude.Maybe Lude.Text,
    importTaskId :: Lude.Maybe Lude.Text,
    importDeletedTime :: Lude.Maybe Lude.Timestamp,
    serverImportFailure :: Lude.Maybe Lude.Int,
    clientRequestToken :: Lude.Maybe Lude.Text,
    importURL :: Lude.Maybe Lude.Text,
    importRequestTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportTask' with the minimum fields required to make a request.
--
-- * 'applicationImportFailure' - The total number of application records in the import file that failed to be imported.
-- * 'applicationImportSuccess' - The total number of application records in the import file that were successfully imported.
-- * 'clientRequestToken' - A unique token used to prevent the same import request from occurring more than once. If you didn't provide a token, a token was automatically generated when the import task request was sent.
-- * 'errorsAndFailedEntriesZip' - A link to a compressed archive folder (in the ZIP format) that contains an error log and a file of failed records. You can use these two files to quickly identify records that failed, why they failed, and correct those records. Afterward, you can upload the corrected file to your Amazon S3 bucket and create another import task request.
--
-- This field also includes authorization information so you can confirm the authenticity of the compressed archive before you download it.
-- If some records failed to be imported we recommend that you correct the records in the failed entries file and then imports that failed entries file. This prevents you from having to correct and update the larger original file and attempt importing it again.
-- * 'importCompletionTime' - The time that the import task request finished, presented in the Unix time stamp format.
-- * 'importDeletedTime' - The time that the import task request was deleted, presented in the Unix time stamp format.
-- * 'importRequestTime' - The time that the import task request was made, presented in the Unix time stamp format.
-- * 'importTaskId' - The unique ID for a specific import task. These IDs aren't globally unique, but they are unique within an AWS account.
-- * 'importURL' - The URL for your import file that you've uploaded to Amazon S3.
-- * 'name' - A descriptive name for an import task. You can use this name to filter future requests related to this import task, such as identifying applications and servers that were included in this import task. We recommend that you use a meaningful name for each import task.
-- * 'serverImportFailure' - The total number of server records in the import file that failed to be imported.
-- * 'serverImportSuccess' - The total number of server records in the import file that were successfully imported.
-- * 'status' - The status of the import task. An import can have the status of @IMPORT_COMPLETE@ and still have some records fail to import from the overall request. More information can be found in the downloadable archive defined in the @errorsAndFailedEntriesZip@ field, or in the Migration Hub management console.
mkImportTask ::
  ImportTask
mkImportTask =
  ImportTask'
    { applicationImportSuccess = Lude.Nothing,
      status = Lude.Nothing,
      serverImportSuccess = Lude.Nothing,
      importCompletionTime = Lude.Nothing,
      name = Lude.Nothing,
      applicationImportFailure = Lude.Nothing,
      errorsAndFailedEntriesZip = Lude.Nothing,
      importTaskId = Lude.Nothing,
      importDeletedTime = Lude.Nothing,
      serverImportFailure = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      importURL = Lude.Nothing,
      importRequestTime = Lude.Nothing
    }

-- | The total number of application records in the import file that were successfully imported.
--
-- /Note:/ Consider using 'applicationImportSuccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itApplicationImportSuccess :: Lens.Lens' ImportTask (Lude.Maybe Lude.Int)
itApplicationImportSuccess = Lens.lens (applicationImportSuccess :: ImportTask -> Lude.Maybe Lude.Int) (\s a -> s {applicationImportSuccess = a} :: ImportTask)
{-# DEPRECATED itApplicationImportSuccess "Use generic-lens or generic-optics with 'applicationImportSuccess' instead." #-}

-- | The status of the import task. An import can have the status of @IMPORT_COMPLETE@ and still have some records fail to import from the overall request. More information can be found in the downloadable archive defined in the @errorsAndFailedEntriesZip@ field, or in the Migration Hub management console.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itStatus :: Lens.Lens' ImportTask (Lude.Maybe ImportStatus)
itStatus = Lens.lens (status :: ImportTask -> Lude.Maybe ImportStatus) (\s a -> s {status = a} :: ImportTask)
{-# DEPRECATED itStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The total number of server records in the import file that were successfully imported.
--
-- /Note:/ Consider using 'serverImportSuccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itServerImportSuccess :: Lens.Lens' ImportTask (Lude.Maybe Lude.Int)
itServerImportSuccess = Lens.lens (serverImportSuccess :: ImportTask -> Lude.Maybe Lude.Int) (\s a -> s {serverImportSuccess = a} :: ImportTask)
{-# DEPRECATED itServerImportSuccess "Use generic-lens or generic-optics with 'serverImportSuccess' instead." #-}

-- | The time that the import task request finished, presented in the Unix time stamp format.
--
-- /Note:/ Consider using 'importCompletionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itImportCompletionTime :: Lens.Lens' ImportTask (Lude.Maybe Lude.Timestamp)
itImportCompletionTime = Lens.lens (importCompletionTime :: ImportTask -> Lude.Maybe Lude.Timestamp) (\s a -> s {importCompletionTime = a} :: ImportTask)
{-# DEPRECATED itImportCompletionTime "Use generic-lens or generic-optics with 'importCompletionTime' instead." #-}

-- | A descriptive name for an import task. You can use this name to filter future requests related to this import task, such as identifying applications and servers that were included in this import task. We recommend that you use a meaningful name for each import task.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itName :: Lens.Lens' ImportTask (Lude.Maybe Lude.Text)
itName = Lens.lens (name :: ImportTask -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ImportTask)
{-# DEPRECATED itName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The total number of application records in the import file that failed to be imported.
--
-- /Note:/ Consider using 'applicationImportFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itApplicationImportFailure :: Lens.Lens' ImportTask (Lude.Maybe Lude.Int)
itApplicationImportFailure = Lens.lens (applicationImportFailure :: ImportTask -> Lude.Maybe Lude.Int) (\s a -> s {applicationImportFailure = a} :: ImportTask)
{-# DEPRECATED itApplicationImportFailure "Use generic-lens or generic-optics with 'applicationImportFailure' instead." #-}

-- | A link to a compressed archive folder (in the ZIP format) that contains an error log and a file of failed records. You can use these two files to quickly identify records that failed, why they failed, and correct those records. Afterward, you can upload the corrected file to your Amazon S3 bucket and create another import task request.
--
-- This field also includes authorization information so you can confirm the authenticity of the compressed archive before you download it.
-- If some records failed to be imported we recommend that you correct the records in the failed entries file and then imports that failed entries file. This prevents you from having to correct and update the larger original file and attempt importing it again.
--
-- /Note:/ Consider using 'errorsAndFailedEntriesZip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itErrorsAndFailedEntriesZip :: Lens.Lens' ImportTask (Lude.Maybe Lude.Text)
itErrorsAndFailedEntriesZip = Lens.lens (errorsAndFailedEntriesZip :: ImportTask -> Lude.Maybe Lude.Text) (\s a -> s {errorsAndFailedEntriesZip = a} :: ImportTask)
{-# DEPRECATED itErrorsAndFailedEntriesZip "Use generic-lens or generic-optics with 'errorsAndFailedEntriesZip' instead." #-}

-- | The unique ID for a specific import task. These IDs aren't globally unique, but they are unique within an AWS account.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itImportTaskId :: Lens.Lens' ImportTask (Lude.Maybe Lude.Text)
itImportTaskId = Lens.lens (importTaskId :: ImportTask -> Lude.Maybe Lude.Text) (\s a -> s {importTaskId = a} :: ImportTask)
{-# DEPRECATED itImportTaskId "Use generic-lens or generic-optics with 'importTaskId' instead." #-}

-- | The time that the import task request was deleted, presented in the Unix time stamp format.
--
-- /Note:/ Consider using 'importDeletedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itImportDeletedTime :: Lens.Lens' ImportTask (Lude.Maybe Lude.Timestamp)
itImportDeletedTime = Lens.lens (importDeletedTime :: ImportTask -> Lude.Maybe Lude.Timestamp) (\s a -> s {importDeletedTime = a} :: ImportTask)
{-# DEPRECATED itImportDeletedTime "Use generic-lens or generic-optics with 'importDeletedTime' instead." #-}

-- | The total number of server records in the import file that failed to be imported.
--
-- /Note:/ Consider using 'serverImportFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itServerImportFailure :: Lens.Lens' ImportTask (Lude.Maybe Lude.Int)
itServerImportFailure = Lens.lens (serverImportFailure :: ImportTask -> Lude.Maybe Lude.Int) (\s a -> s {serverImportFailure = a} :: ImportTask)
{-# DEPRECATED itServerImportFailure "Use generic-lens or generic-optics with 'serverImportFailure' instead." #-}

-- | A unique token used to prevent the same import request from occurring more than once. If you didn't provide a token, a token was automatically generated when the import task request was sent.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itClientRequestToken :: Lens.Lens' ImportTask (Lude.Maybe Lude.Text)
itClientRequestToken = Lens.lens (clientRequestToken :: ImportTask -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: ImportTask)
{-# DEPRECATED itClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The URL for your import file that you've uploaded to Amazon S3.
--
-- /Note:/ Consider using 'importURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itImportURL :: Lens.Lens' ImportTask (Lude.Maybe Lude.Text)
itImportURL = Lens.lens (importURL :: ImportTask -> Lude.Maybe Lude.Text) (\s a -> s {importURL = a} :: ImportTask)
{-# DEPRECATED itImportURL "Use generic-lens or generic-optics with 'importURL' instead." #-}

-- | The time that the import task request was made, presented in the Unix time stamp format.
--
-- /Note:/ Consider using 'importRequestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itImportRequestTime :: Lens.Lens' ImportTask (Lude.Maybe Lude.Timestamp)
itImportRequestTime = Lens.lens (importRequestTime :: ImportTask -> Lude.Maybe Lude.Timestamp) (\s a -> s {importRequestTime = a} :: ImportTask)
{-# DEPRECATED itImportRequestTime "Use generic-lens or generic-optics with 'importRequestTime' instead." #-}

instance Lude.FromJSON ImportTask where
  parseJSON =
    Lude.withObject
      "ImportTask"
      ( \x ->
          ImportTask'
            Lude.<$> (x Lude..:? "applicationImportSuccess")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "serverImportSuccess")
            Lude.<*> (x Lude..:? "importCompletionTime")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "applicationImportFailure")
            Lude.<*> (x Lude..:? "errorsAndFailedEntriesZip")
            Lude.<*> (x Lude..:? "importTaskId")
            Lude.<*> (x Lude..:? "importDeletedTime")
            Lude.<*> (x Lude..:? "serverImportFailure")
            Lude.<*> (x Lude..:? "clientRequestToken")
            Lude.<*> (x Lude..:? "importUrl")
            Lude.<*> (x Lude..:? "importRequestTime")
      )
