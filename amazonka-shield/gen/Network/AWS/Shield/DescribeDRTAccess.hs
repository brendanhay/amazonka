{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DescribeDRTAccess
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current role and list of Amazon S3 log buckets used by the DDoS Response team (DRT) to access your AWS account while assisting with attack mitigation.
--
--
module Network.AWS.Shield.DescribeDRTAccess
    (
    -- * Creating a Request
      describeDRTAccess
    , DescribeDRTAccess

    -- * Destructuring the Response
    , describeDRTAccessResponse
    , DescribeDRTAccessResponse
    -- * Response Lenses
    , ddrtarsLogBucketList
    , ddrtarsRoleARN
    , ddrtarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types
import Network.AWS.Shield.Types.Product

-- | /See:/ 'describeDRTAccess' smart constructor.
data DescribeDRTAccess =
  DescribeDRTAccess'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDRTAccess' with the minimum fields required to make a request.
--
describeDRTAccess
    :: DescribeDRTAccess
describeDRTAccess = DescribeDRTAccess'


instance AWSRequest DescribeDRTAccess where
        type Rs DescribeDRTAccess = DescribeDRTAccessResponse
        request = postJSON shield
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDRTAccessResponse' <$>
                   (x .?> "LogBucketList" .!@ mempty) <*>
                     (x .?> "RoleArn")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDRTAccess where

instance NFData DescribeDRTAccess where

instance ToHeaders DescribeDRTAccess where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSShield_20160616.DescribeDRTAccess" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeDRTAccess where
        toJSON = const (Object mempty)

instance ToPath DescribeDRTAccess where
        toPath = const "/"

instance ToQuery DescribeDRTAccess where
        toQuery = const mempty

-- | /See:/ 'describeDRTAccessResponse' smart constructor.
data DescribeDRTAccessResponse = DescribeDRTAccessResponse'
  { _ddrtarsLogBucketList  :: !(Maybe [Text])
  , _ddrtarsRoleARN        :: !(Maybe Text)
  , _ddrtarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDRTAccessResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrtarsLogBucketList' - The list of Amazon S3 buckets accessed by the DRT.
--
-- * 'ddrtarsRoleARN' - The Amazon Resource Name (ARN) of the role the DRT used to access your AWS account.
--
-- * 'ddrtarsResponseStatus' - -- | The response status code.
describeDRTAccessResponse
    :: Int -- ^ 'ddrtarsResponseStatus'
    -> DescribeDRTAccessResponse
describeDRTAccessResponse pResponseStatus_ =
  DescribeDRTAccessResponse'
    { _ddrtarsLogBucketList = Nothing
    , _ddrtarsRoleARN = Nothing
    , _ddrtarsResponseStatus = pResponseStatus_
    }


-- | The list of Amazon S3 buckets accessed by the DRT.
ddrtarsLogBucketList :: Lens' DescribeDRTAccessResponse [Text]
ddrtarsLogBucketList = lens _ddrtarsLogBucketList (\ s a -> s{_ddrtarsLogBucketList = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the role the DRT used to access your AWS account.
ddrtarsRoleARN :: Lens' DescribeDRTAccessResponse (Maybe Text)
ddrtarsRoleARN = lens _ddrtarsRoleARN (\ s a -> s{_ddrtarsRoleARN = a})

-- | -- | The response status code.
ddrtarsResponseStatus :: Lens' DescribeDRTAccessResponse Int
ddrtarsResponseStatus = lens _ddrtarsResponseStatus (\ s a -> s{_ddrtarsResponseStatus = a})

instance NFData DescribeDRTAccessResponse where
