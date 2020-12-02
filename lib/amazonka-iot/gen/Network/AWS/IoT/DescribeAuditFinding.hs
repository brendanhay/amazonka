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
-- Module      : Network.AWS.IoT.DescribeAuditFinding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a single audit finding. Properties include the reason for noncompliance, the severity of the issue, and when the audit that returned the finding was started.
module Network.AWS.IoT.DescribeAuditFinding
  ( -- * Creating a Request
    describeAuditFinding,
    DescribeAuditFinding,

    -- * Request Lenses
    dafFindingId,

    -- * Destructuring the Response
    describeAuditFindingResponse,
    DescribeAuditFindingResponse,

    -- * Response Lenses
    dafrsFinding,
    dafrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAuditFinding' smart constructor.
newtype DescribeAuditFinding = DescribeAuditFinding'
  { _dafFindingId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAuditFinding' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dafFindingId' - A unique identifier for a single audit finding. You can use this identifier to apply mitigation actions to the finding.
describeAuditFinding ::
  -- | 'dafFindingId'
  Text ->
  DescribeAuditFinding
describeAuditFinding pFindingId_ =
  DescribeAuditFinding' {_dafFindingId = pFindingId_}

-- | A unique identifier for a single audit finding. You can use this identifier to apply mitigation actions to the finding.
dafFindingId :: Lens' DescribeAuditFinding Text
dafFindingId = lens _dafFindingId (\s a -> s {_dafFindingId = a})

instance AWSRequest DescribeAuditFinding where
  type Rs DescribeAuditFinding = DescribeAuditFindingResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          DescribeAuditFindingResponse'
            <$> (x .?> "finding") <*> (pure (fromEnum s))
      )

instance Hashable DescribeAuditFinding

instance NFData DescribeAuditFinding

instance ToHeaders DescribeAuditFinding where
  toHeaders = const mempty

instance ToPath DescribeAuditFinding where
  toPath DescribeAuditFinding' {..} =
    mconcat ["/audit/findings/", toBS _dafFindingId]

instance ToQuery DescribeAuditFinding where
  toQuery = const mempty

-- | /See:/ 'describeAuditFindingResponse' smart constructor.
data DescribeAuditFindingResponse = DescribeAuditFindingResponse'
  { _dafrsFinding ::
      !(Maybe AuditFinding),
    _dafrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAuditFindingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dafrsFinding' - Undocumented member.
--
-- * 'dafrsResponseStatus' - -- | The response status code.
describeAuditFindingResponse ::
  -- | 'dafrsResponseStatus'
  Int ->
  DescribeAuditFindingResponse
describeAuditFindingResponse pResponseStatus_ =
  DescribeAuditFindingResponse'
    { _dafrsFinding = Nothing,
      _dafrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dafrsFinding :: Lens' DescribeAuditFindingResponse (Maybe AuditFinding)
dafrsFinding = lens _dafrsFinding (\s a -> s {_dafrsFinding = a})

-- | -- | The response status code.
dafrsResponseStatus :: Lens' DescribeAuditFindingResponse Int
dafrsResponseStatus = lens _dafrsResponseStatus (\s a -> s {_dafrsResponseStatus = a})

instance NFData DescribeAuditFindingResponse
