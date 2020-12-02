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
-- Module      : Network.AWS.SageMaker.DescribeWorkforce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists private workforce information, including workforce name, Amazon Resource Name (ARN), and, if applicable, allowed IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ). Allowable IP address ranges are the IP addresses that workers can use to access tasks.
--
--
-- /Important:/ This operation applies only to private workforces.
module Network.AWS.SageMaker.DescribeWorkforce
  ( -- * Creating a Request
    describeWorkforce,
    DescribeWorkforce,

    -- * Request Lenses
    dWorkforceName,

    -- * Destructuring the Response
    describeWorkforceResponse,
    DescribeWorkforceResponse,

    -- * Response Lenses
    dwrsResponseStatus,
    dwrsWorkforce,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeWorkforce' smart constructor.
newtype DescribeWorkforce = DescribeWorkforce'
  { _dWorkforceName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeWorkforce' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dWorkforceName' - The name of the private workforce whose access you want to restrict. @WorkforceName@ is automatically set to @default@ when a workforce is created and cannot be modified.
describeWorkforce ::
  -- | 'dWorkforceName'
  Text ->
  DescribeWorkforce
describeWorkforce pWorkforceName_ =
  DescribeWorkforce' {_dWorkforceName = pWorkforceName_}

-- | The name of the private workforce whose access you want to restrict. @WorkforceName@ is automatically set to @default@ when a workforce is created and cannot be modified.
dWorkforceName :: Lens' DescribeWorkforce Text
dWorkforceName = lens _dWorkforceName (\s a -> s {_dWorkforceName = a})

instance AWSRequest DescribeWorkforce where
  type Rs DescribeWorkforce = DescribeWorkforceResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeWorkforceResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "Workforce")
      )

instance Hashable DescribeWorkforce

instance NFData DescribeWorkforce

instance ToHeaders DescribeWorkforce where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DescribeWorkforce" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeWorkforce where
  toJSON DescribeWorkforce' {..} =
    object (catMaybes [Just ("WorkforceName" .= _dWorkforceName)])

instance ToPath DescribeWorkforce where
  toPath = const "/"

instance ToQuery DescribeWorkforce where
  toQuery = const mempty

-- | /See:/ 'describeWorkforceResponse' smart constructor.
data DescribeWorkforceResponse = DescribeWorkforceResponse'
  { _dwrsResponseStatus ::
      !Int,
    _dwrsWorkforce :: !Workforce
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeWorkforceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwrsResponseStatus' - -- | The response status code.
--
-- * 'dwrsWorkforce' - A single private workforce, which is automatically created when you create your first private work team. You can create one private work force in each AWS Region. By default, any workforce-related API operation used in a specific region will apply to the workforce created in that region. To learn how to create a private workforce, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce> .
describeWorkforceResponse ::
  -- | 'dwrsResponseStatus'
  Int ->
  -- | 'dwrsWorkforce'
  Workforce ->
  DescribeWorkforceResponse
describeWorkforceResponse pResponseStatus_ pWorkforce_ =
  DescribeWorkforceResponse'
    { _dwrsResponseStatus =
        pResponseStatus_,
      _dwrsWorkforce = pWorkforce_
    }

-- | -- | The response status code.
dwrsResponseStatus :: Lens' DescribeWorkforceResponse Int
dwrsResponseStatus = lens _dwrsResponseStatus (\s a -> s {_dwrsResponseStatus = a})

-- | A single private workforce, which is automatically created when you create your first private work team. You can create one private work force in each AWS Region. By default, any workforce-related API operation used in a specific region will apply to the workforce created in that region. To learn how to create a private workforce, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce> .
dwrsWorkforce :: Lens' DescribeWorkforceResponse Workforce
dwrsWorkforce = lens _dwrsWorkforce (\s a -> s {_dwrsWorkforce = a})

instance NFData DescribeWorkforceResponse
