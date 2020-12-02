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
-- Module      : Network.AWS.MediaStore.PutMetricPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The metric policy that you want to add to the container. A metric policy allows AWS Elemental MediaStore to send metrics to Amazon CloudWatch. It takes up to 20 minutes for the new policy to take effect.
module Network.AWS.MediaStore.PutMetricPolicy
  ( -- * Creating a Request
    putMetricPolicy,
    PutMetricPolicy,

    -- * Request Lenses
    pmpContainerName,
    pmpMetricPolicy,

    -- * Destructuring the Response
    putMetricPolicyResponse,
    PutMetricPolicyResponse,

    -- * Response Lenses
    pmprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putMetricPolicy' smart constructor.
data PutMetricPolicy = PutMetricPolicy'
  { _pmpContainerName :: !Text,
    _pmpMetricPolicy :: !MetricPolicy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutMetricPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmpContainerName' - The name of the container that you want to add the metric policy to.
--
-- * 'pmpMetricPolicy' - The metric policy that you want to associate with the container. In the policy, you must indicate whether you want MediaStore to send container-level metrics. You can also include up to five rules to define groups of objects that you want MediaStore to send object-level metrics for. If you include rules in the policy, construct each rule with both of the following:     * An object group that defines which objects to include in the group. The definition can be a path or a file name, but it can't have more than 900 characters. Valid characters are: a-z, A-Z, 0-9, _ (underscore), = (equal), : (colon), . (period), - (hyphen), ~ (tilde), / (forward slash), and * (asterisk). Wildcards (*) are acceptable.     * An object group name that allows you to refer to the object group. The name can't have more than 30 characters. Valid characters are: a-z, A-Z, 0-9, and _ (underscore).
putMetricPolicy ::
  -- | 'pmpContainerName'
  Text ->
  -- | 'pmpMetricPolicy'
  MetricPolicy ->
  PutMetricPolicy
putMetricPolicy pContainerName_ pMetricPolicy_ =
  PutMetricPolicy'
    { _pmpContainerName = pContainerName_,
      _pmpMetricPolicy = pMetricPolicy_
    }

-- | The name of the container that you want to add the metric policy to.
pmpContainerName :: Lens' PutMetricPolicy Text
pmpContainerName = lens _pmpContainerName (\s a -> s {_pmpContainerName = a})

-- | The metric policy that you want to associate with the container. In the policy, you must indicate whether you want MediaStore to send container-level metrics. You can also include up to five rules to define groups of objects that you want MediaStore to send object-level metrics for. If you include rules in the policy, construct each rule with both of the following:     * An object group that defines which objects to include in the group. The definition can be a path or a file name, but it can't have more than 900 characters. Valid characters are: a-z, A-Z, 0-9, _ (underscore), = (equal), : (colon), . (period), - (hyphen), ~ (tilde), / (forward slash), and * (asterisk). Wildcards (*) are acceptable.     * An object group name that allows you to refer to the object group. The name can't have more than 30 characters. Valid characters are: a-z, A-Z, 0-9, and _ (underscore).
pmpMetricPolicy :: Lens' PutMetricPolicy MetricPolicy
pmpMetricPolicy = lens _pmpMetricPolicy (\s a -> s {_pmpMetricPolicy = a})

instance AWSRequest PutMetricPolicy where
  type Rs PutMetricPolicy = PutMetricPolicyResponse
  request = postJSON mediaStore
  response =
    receiveEmpty
      (\s h x -> PutMetricPolicyResponse' <$> (pure (fromEnum s)))

instance Hashable PutMetricPolicy

instance NFData PutMetricPolicy

instance ToHeaders PutMetricPolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("MediaStore_20170901.PutMetricPolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutMetricPolicy where
  toJSON PutMetricPolicy' {..} =
    object
      ( catMaybes
          [ Just ("ContainerName" .= _pmpContainerName),
            Just ("MetricPolicy" .= _pmpMetricPolicy)
          ]
      )

instance ToPath PutMetricPolicy where
  toPath = const "/"

instance ToQuery PutMetricPolicy where
  toQuery = const mempty

-- | /See:/ 'putMetricPolicyResponse' smart constructor.
newtype PutMetricPolicyResponse = PutMetricPolicyResponse'
  { _pmprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutMetricPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmprsResponseStatus' - -- | The response status code.
putMetricPolicyResponse ::
  -- | 'pmprsResponseStatus'
  Int ->
  PutMetricPolicyResponse
putMetricPolicyResponse pResponseStatus_ =
  PutMetricPolicyResponse' {_pmprsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
pmprsResponseStatus :: Lens' PutMetricPolicyResponse Int
pmprsResponseStatus = lens _pmprsResponseStatus (\s a -> s {_pmprsResponseStatus = a})

instance NFData PutMetricPolicyResponse
