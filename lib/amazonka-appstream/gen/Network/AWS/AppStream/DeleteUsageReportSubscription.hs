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
-- Module      : Network.AWS.AppStream.DeleteUsageReportSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables usage report generation.
module Network.AWS.AppStream.DeleteUsageReportSubscription
  ( -- * Creating a Request
    deleteUsageReportSubscription,
    DeleteUsageReportSubscription,

    -- * Destructuring the Response
    deleteUsageReportSubscriptionResponse,
    DeleteUsageReportSubscriptionResponse,

    -- * Response Lenses
    dursrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUsageReportSubscription' smart constructor.
data DeleteUsageReportSubscription = DeleteUsageReportSubscription'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUsageReportSubscription' with the minimum fields required to make a request.
deleteUsageReportSubscription ::
  DeleteUsageReportSubscription
deleteUsageReportSubscription = DeleteUsageReportSubscription'

instance AWSRequest DeleteUsageReportSubscription where
  type
    Rs DeleteUsageReportSubscription =
      DeleteUsageReportSubscriptionResponse
  request = postJSON appStream
  response =
    receiveEmpty
      ( \s h x ->
          DeleteUsageReportSubscriptionResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteUsageReportSubscription

instance NFData DeleteUsageReportSubscription

instance ToHeaders DeleteUsageReportSubscription where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "PhotonAdminProxyService.DeleteUsageReportSubscription" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteUsageReportSubscription where
  toJSON = const (Object mempty)

instance ToPath DeleteUsageReportSubscription where
  toPath = const "/"

instance ToQuery DeleteUsageReportSubscription where
  toQuery = const mempty

-- | /See:/ 'deleteUsageReportSubscriptionResponse' smart constructor.
newtype DeleteUsageReportSubscriptionResponse = DeleteUsageReportSubscriptionResponse'
  { _dursrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUsageReportSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dursrsResponseStatus' - -- | The response status code.
deleteUsageReportSubscriptionResponse ::
  -- | 'dursrsResponseStatus'
  Int ->
  DeleteUsageReportSubscriptionResponse
deleteUsageReportSubscriptionResponse pResponseStatus_ =
  DeleteUsageReportSubscriptionResponse'
    { _dursrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dursrsResponseStatus :: Lens' DeleteUsageReportSubscriptionResponse Int
dursrsResponseStatus = lens _dursrsResponseStatus (\s a -> s {_dursrsResponseStatus = a})

instance NFData DeleteUsageReportSubscriptionResponse
