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
-- Module      : Network.AWS.Redshift.DeleteUsageLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a usage limit from a cluster.
module Network.AWS.Redshift.DeleteUsageLimit
  ( -- * Creating a Request
    deleteUsageLimit,
    DeleteUsageLimit,

    -- * Request Lenses
    dulUsageLimitId,

    -- * Destructuring the Response
    deleteUsageLimitResponse,
    DeleteUsageLimitResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUsageLimit' smart constructor.
newtype DeleteUsageLimit = DeleteUsageLimit'
  { _dulUsageLimitId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUsageLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dulUsageLimitId' - The identifier of the usage limit to delete.
deleteUsageLimit ::
  -- | 'dulUsageLimitId'
  Text ->
  DeleteUsageLimit
deleteUsageLimit pUsageLimitId_ =
  DeleteUsageLimit' {_dulUsageLimitId = pUsageLimitId_}

-- | The identifier of the usage limit to delete.
dulUsageLimitId :: Lens' DeleteUsageLimit Text
dulUsageLimitId = lens _dulUsageLimitId (\s a -> s {_dulUsageLimitId = a})

instance AWSRequest DeleteUsageLimit where
  type Rs DeleteUsageLimit = DeleteUsageLimitResponse
  request = postQuery redshift
  response = receiveNull DeleteUsageLimitResponse'

instance Hashable DeleteUsageLimit

instance NFData DeleteUsageLimit

instance ToHeaders DeleteUsageLimit where
  toHeaders = const mempty

instance ToPath DeleteUsageLimit where
  toPath = const "/"

instance ToQuery DeleteUsageLimit where
  toQuery DeleteUsageLimit' {..} =
    mconcat
      [ "Action" =: ("DeleteUsageLimit" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "UsageLimitId" =: _dulUsageLimitId
      ]

-- | /See:/ 'deleteUsageLimitResponse' smart constructor.
data DeleteUsageLimitResponse = DeleteUsageLimitResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUsageLimitResponse' with the minimum fields required to make a request.
deleteUsageLimitResponse ::
  DeleteUsageLimitResponse
deleteUsageLimitResponse = DeleteUsageLimitResponse'

instance NFData DeleteUsageLimitResponse
