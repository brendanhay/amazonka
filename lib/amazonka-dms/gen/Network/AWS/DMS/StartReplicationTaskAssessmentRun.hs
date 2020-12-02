{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.StartReplicationTaskAssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new premigration assessment run for one or more individual assessments of a migration task.
--
--
-- The assessments that you can specify depend on the source and target database engine and the migration type defined for the given task. To run this operation, your migration task must already be created. After you run this operation, you can review the status of each individual assessment. You can also run the migration task manually after the assessment run and its individual assessments complete.
module Network.AWS.DMS.StartReplicationTaskAssessmentRun
  ( -- * Creating a Request
    startReplicationTaskAssessmentRun,
    StartReplicationTaskAssessmentRun,

    -- * Request Lenses
    srtarIncludeOnly,
    srtarResultKMSKeyARN,
    srtarResultLocationFolder,
    srtarResultEncryptionMode,
    srtarExclude,
    srtarReplicationTaskARN,
    srtarServiceAccessRoleARN,
    srtarResultLocationBucket,
    srtarAssessmentRunName,

    -- * Destructuring the Response
    startReplicationTaskAssessmentRunResponse,
    StartReplicationTaskAssessmentRunResponse,

    -- * Response Lenses
    srtarrsReplicationTaskAssessmentRun,
    srtarrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'startReplicationTaskAssessmentRun' smart constructor.
data StartReplicationTaskAssessmentRun = StartReplicationTaskAssessmentRun'
  { _srtarIncludeOnly ::
      !(Maybe [Text]),
    _srtarResultKMSKeyARN ::
      !(Maybe Text),
    _srtarResultLocationFolder ::
      !(Maybe Text),
    _srtarResultEncryptionMode ::
      !(Maybe Text),
    _srtarExclude ::
      !(Maybe [Text]),
    _srtarReplicationTaskARN ::
      !Text,
    _srtarServiceAccessRoleARN ::
      !Text,
    _srtarResultLocationBucket ::
      !Text,
    _srtarAssessmentRunName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartReplicationTaskAssessmentRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srtarIncludeOnly' - Space-separated list of names for specific individual assessments that you want to include. These names come from the default list of individual assessments that AWS DMS supports for the associated migration task. This task is specified by @ReplicationTaskArn@ .
--
-- * 'srtarResultKMSKeyARN' - ARN of a custom KMS encryption key that you specify when you set @ResultEncryptionMode@ to @"SSE_KMS@ ".
--
-- * 'srtarResultLocationFolder' - Folder within an Amazon S3 bucket where you want AWS DMS to store the results of this assessment run.
--
-- * 'srtarResultEncryptionMode' - Encryption mode that you can specify to encrypt the results of this assessment run. If you don't specify this request parameter, AWS DMS stores the assessment run results without encryption. You can specify one of the options following:     * @"SSE_S3"@ – The server-side encryption provided as a default by Amazon S3.     * @"SSE_KMS"@ – AWS Key Management Service (AWS KMS) encryption. This encryption can use either a custom KMS encryption key that you specify or the default KMS encryption key that DMS provides.
--
-- * 'srtarExclude' - Space-separated list of names for specific individual assessments that you want to exclude. These names come from the default list of individual assessments that AWS DMS supports for the associated migration task. This task is specified by @ReplicationTaskArn@ .
--
-- * 'srtarReplicationTaskARN' - Amazon Resource Name (ARN) of the migration task associated with the premigration assessment run that you want to start.
--
-- * 'srtarServiceAccessRoleARN' - ARN of a service role needed to start the assessment run.
--
-- * 'srtarResultLocationBucket' - Amazon S3 bucket where you want AWS DMS to store the results of this assessment run.
--
-- * 'srtarAssessmentRunName' - Unique name to identify the assessment run.
startReplicationTaskAssessmentRun ::
  -- | 'srtarReplicationTaskARN'
  Text ->
  -- | 'srtarServiceAccessRoleARN'
  Text ->
  -- | 'srtarResultLocationBucket'
  Text ->
  -- | 'srtarAssessmentRunName'
  Text ->
  StartReplicationTaskAssessmentRun
startReplicationTaskAssessmentRun
  pReplicationTaskARN_
  pServiceAccessRoleARN_
  pResultLocationBucket_
  pAssessmentRunName_ =
    StartReplicationTaskAssessmentRun'
      { _srtarIncludeOnly = Nothing,
        _srtarResultKMSKeyARN = Nothing,
        _srtarResultLocationFolder = Nothing,
        _srtarResultEncryptionMode = Nothing,
        _srtarExclude = Nothing,
        _srtarReplicationTaskARN = pReplicationTaskARN_,
        _srtarServiceAccessRoleARN = pServiceAccessRoleARN_,
        _srtarResultLocationBucket = pResultLocationBucket_,
        _srtarAssessmentRunName = pAssessmentRunName_
      }

-- | Space-separated list of names for specific individual assessments that you want to include. These names come from the default list of individual assessments that AWS DMS supports for the associated migration task. This task is specified by @ReplicationTaskArn@ .
srtarIncludeOnly :: Lens' StartReplicationTaskAssessmentRun [Text]
srtarIncludeOnly = lens _srtarIncludeOnly (\s a -> s {_srtarIncludeOnly = a}) . _Default . _Coerce

-- | ARN of a custom KMS encryption key that you specify when you set @ResultEncryptionMode@ to @"SSE_KMS@ ".
srtarResultKMSKeyARN :: Lens' StartReplicationTaskAssessmentRun (Maybe Text)
srtarResultKMSKeyARN = lens _srtarResultKMSKeyARN (\s a -> s {_srtarResultKMSKeyARN = a})

-- | Folder within an Amazon S3 bucket where you want AWS DMS to store the results of this assessment run.
srtarResultLocationFolder :: Lens' StartReplicationTaskAssessmentRun (Maybe Text)
srtarResultLocationFolder = lens _srtarResultLocationFolder (\s a -> s {_srtarResultLocationFolder = a})

-- | Encryption mode that you can specify to encrypt the results of this assessment run. If you don't specify this request parameter, AWS DMS stores the assessment run results without encryption. You can specify one of the options following:     * @"SSE_S3"@ – The server-side encryption provided as a default by Amazon S3.     * @"SSE_KMS"@ – AWS Key Management Service (AWS KMS) encryption. This encryption can use either a custom KMS encryption key that you specify or the default KMS encryption key that DMS provides.
srtarResultEncryptionMode :: Lens' StartReplicationTaskAssessmentRun (Maybe Text)
srtarResultEncryptionMode = lens _srtarResultEncryptionMode (\s a -> s {_srtarResultEncryptionMode = a})

-- | Space-separated list of names for specific individual assessments that you want to exclude. These names come from the default list of individual assessments that AWS DMS supports for the associated migration task. This task is specified by @ReplicationTaskArn@ .
srtarExclude :: Lens' StartReplicationTaskAssessmentRun [Text]
srtarExclude = lens _srtarExclude (\s a -> s {_srtarExclude = a}) . _Default . _Coerce

-- | Amazon Resource Name (ARN) of the migration task associated with the premigration assessment run that you want to start.
srtarReplicationTaskARN :: Lens' StartReplicationTaskAssessmentRun Text
srtarReplicationTaskARN = lens _srtarReplicationTaskARN (\s a -> s {_srtarReplicationTaskARN = a})

-- | ARN of a service role needed to start the assessment run.
srtarServiceAccessRoleARN :: Lens' StartReplicationTaskAssessmentRun Text
srtarServiceAccessRoleARN = lens _srtarServiceAccessRoleARN (\s a -> s {_srtarServiceAccessRoleARN = a})

-- | Amazon S3 bucket where you want AWS DMS to store the results of this assessment run.
srtarResultLocationBucket :: Lens' StartReplicationTaskAssessmentRun Text
srtarResultLocationBucket = lens _srtarResultLocationBucket (\s a -> s {_srtarResultLocationBucket = a})

-- | Unique name to identify the assessment run.
srtarAssessmentRunName :: Lens' StartReplicationTaskAssessmentRun Text
srtarAssessmentRunName = lens _srtarAssessmentRunName (\s a -> s {_srtarAssessmentRunName = a})

instance AWSRequest StartReplicationTaskAssessmentRun where
  type
    Rs StartReplicationTaskAssessmentRun =
      StartReplicationTaskAssessmentRunResponse
  request = postJSON dms
  response =
    receiveJSON
      ( \s h x ->
          StartReplicationTaskAssessmentRunResponse'
            <$> (x .?> "ReplicationTaskAssessmentRun") <*> (pure (fromEnum s))
      )

instance Hashable StartReplicationTaskAssessmentRun

instance NFData StartReplicationTaskAssessmentRun

instance ToHeaders StartReplicationTaskAssessmentRun where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonDMSv20160101.StartReplicationTaskAssessmentRun" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartReplicationTaskAssessmentRun where
  toJSON StartReplicationTaskAssessmentRun' {..} =
    object
      ( catMaybes
          [ ("IncludeOnly" .=) <$> _srtarIncludeOnly,
            ("ResultKmsKeyArn" .=) <$> _srtarResultKMSKeyARN,
            ("ResultLocationFolder" .=) <$> _srtarResultLocationFolder,
            ("ResultEncryptionMode" .=) <$> _srtarResultEncryptionMode,
            ("Exclude" .=) <$> _srtarExclude,
            Just ("ReplicationTaskArn" .= _srtarReplicationTaskARN),
            Just ("ServiceAccessRoleArn" .= _srtarServiceAccessRoleARN),
            Just ("ResultLocationBucket" .= _srtarResultLocationBucket),
            Just ("AssessmentRunName" .= _srtarAssessmentRunName)
          ]
      )

instance ToPath StartReplicationTaskAssessmentRun where
  toPath = const "/"

instance ToQuery StartReplicationTaskAssessmentRun where
  toQuery = const mempty

-- |
--
--
--
-- /See:/ 'startReplicationTaskAssessmentRunResponse' smart constructor.
data StartReplicationTaskAssessmentRunResponse = StartReplicationTaskAssessmentRunResponse'
  { _srtarrsReplicationTaskAssessmentRun ::
      !( Maybe
           ReplicationTaskAssessmentRun
       ),
    _srtarrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'StartReplicationTaskAssessmentRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srtarrsReplicationTaskAssessmentRun' - The premigration assessment run that was started.
--
-- * 'srtarrsResponseStatus' - -- | The response status code.
startReplicationTaskAssessmentRunResponse ::
  -- | 'srtarrsResponseStatus'
  Int ->
  StartReplicationTaskAssessmentRunResponse
startReplicationTaskAssessmentRunResponse pResponseStatus_ =
  StartReplicationTaskAssessmentRunResponse'
    { _srtarrsReplicationTaskAssessmentRun =
        Nothing,
      _srtarrsResponseStatus = pResponseStatus_
    }

-- | The premigration assessment run that was started.
srtarrsReplicationTaskAssessmentRun :: Lens' StartReplicationTaskAssessmentRunResponse (Maybe ReplicationTaskAssessmentRun)
srtarrsReplicationTaskAssessmentRun = lens _srtarrsReplicationTaskAssessmentRun (\s a -> s {_srtarrsReplicationTaskAssessmentRun = a})

-- | -- | The response status code.
srtarrsResponseStatus :: Lens' StartReplicationTaskAssessmentRunResponse Int
srtarrsResponseStatus = lens _srtarrsResponseStatus (\s a -> s {_srtarrsResponseStatus = a})

instance NFData StartReplicationTaskAssessmentRunResponse
