{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.BatchDelete
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts delete of resources.
module Network.AWS.MediaLive.BatchDelete
  ( -- * Creating a Request
    BatchDelete' (..),
    newBatchDelete',

    -- * Request Lenses
    batchDelete'_inputSecurityGroupIds,
    batchDelete'_multiplexIds,
    batchDelete'_inputIds,
    batchDelete'_channelIds,

    -- * Destructuring the Response
    BatchDeleteResponse (..),
    newBatchDeleteResponse,

    -- * Response Lenses
    batchDeleteResponse_successful,
    batchDeleteResponse_failed,
    batchDeleteResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to delete resources
--
-- /See:/ 'newBatchDelete'' smart constructor.
data BatchDelete' = BatchDelete''
  { -- | List of input security group IDs
    inputSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | List of multiplex IDs
    multiplexIds :: Prelude.Maybe [Prelude.Text],
    -- | List of input IDs
    inputIds :: Prelude.Maybe [Prelude.Text],
    -- | List of channel IDs
    channelIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDelete'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputSecurityGroupIds', 'batchDelete'_inputSecurityGroupIds' - List of input security group IDs
--
-- 'multiplexIds', 'batchDelete'_multiplexIds' - List of multiplex IDs
--
-- 'inputIds', 'batchDelete'_inputIds' - List of input IDs
--
-- 'channelIds', 'batchDelete'_channelIds' - List of channel IDs
newBatchDelete' ::
  BatchDelete'
newBatchDelete' =
  BatchDelete''
    { inputSecurityGroupIds =
        Prelude.Nothing,
      multiplexIds = Prelude.Nothing,
      inputIds = Prelude.Nothing,
      channelIds = Prelude.Nothing
    }

-- | List of input security group IDs
batchDelete'_inputSecurityGroupIds :: Lens.Lens' BatchDelete' (Prelude.Maybe [Prelude.Text])
batchDelete'_inputSecurityGroupIds = Lens.lens (\BatchDelete'' {inputSecurityGroupIds} -> inputSecurityGroupIds) (\s@BatchDelete'' {} a -> s {inputSecurityGroupIds = a} :: BatchDelete') Prelude.. Lens.mapping Lens._Coerce

-- | List of multiplex IDs
batchDelete'_multiplexIds :: Lens.Lens' BatchDelete' (Prelude.Maybe [Prelude.Text])
batchDelete'_multiplexIds = Lens.lens (\BatchDelete'' {multiplexIds} -> multiplexIds) (\s@BatchDelete'' {} a -> s {multiplexIds = a} :: BatchDelete') Prelude.. Lens.mapping Lens._Coerce

-- | List of input IDs
batchDelete'_inputIds :: Lens.Lens' BatchDelete' (Prelude.Maybe [Prelude.Text])
batchDelete'_inputIds = Lens.lens (\BatchDelete'' {inputIds} -> inputIds) (\s@BatchDelete'' {} a -> s {inputIds = a} :: BatchDelete') Prelude.. Lens.mapping Lens._Coerce

-- | List of channel IDs
batchDelete'_channelIds :: Lens.Lens' BatchDelete' (Prelude.Maybe [Prelude.Text])
batchDelete'_channelIds = Lens.lens (\BatchDelete'' {channelIds} -> channelIds) (\s@BatchDelete'' {} a -> s {channelIds = a} :: BatchDelete') Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSRequest BatchDelete' where
  type AWSResponse BatchDelete' = BatchDeleteResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteResponse'
            Prelude.<$> (x Core..?> "successful" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "failed" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDelete'

instance Prelude.NFData BatchDelete'

instance Core.ToHeaders BatchDelete' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchDelete' where
  toJSON BatchDelete'' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("inputSecurityGroupIds" Core..=)
              Prelude.<$> inputSecurityGroupIds,
            ("multiplexIds" Core..=) Prelude.<$> multiplexIds,
            ("inputIds" Core..=) Prelude.<$> inputIds,
            ("channelIds" Core..=) Prelude.<$> channelIds
          ]
      )

instance Core.ToPath BatchDelete' where
  toPath = Prelude.const "/prod/batch/delete"

instance Core.ToQuery BatchDelete' where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for BatchDeleteResponse
--
-- /See:/ 'newBatchDeleteResponse' smart constructor.
data BatchDeleteResponse = BatchDeleteResponse'
  { -- | List of successful operations
    successful :: Prelude.Maybe [BatchSuccessfulResultModel],
    -- | List of failed operations
    failed :: Prelude.Maybe [BatchFailedResultModel],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successful', 'batchDeleteResponse_successful' - List of successful operations
--
-- 'failed', 'batchDeleteResponse_failed' - List of failed operations
--
-- 'httpStatus', 'batchDeleteResponse_httpStatus' - The response's http status code.
newBatchDeleteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteResponse
newBatchDeleteResponse pHttpStatus_ =
  BatchDeleteResponse'
    { successful = Prelude.Nothing,
      failed = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of successful operations
batchDeleteResponse_successful :: Lens.Lens' BatchDeleteResponse (Prelude.Maybe [BatchSuccessfulResultModel])
batchDeleteResponse_successful = Lens.lens (\BatchDeleteResponse' {successful} -> successful) (\s@BatchDeleteResponse' {} a -> s {successful = a} :: BatchDeleteResponse) Prelude.. Lens.mapping Lens._Coerce

-- | List of failed operations
batchDeleteResponse_failed :: Lens.Lens' BatchDeleteResponse (Prelude.Maybe [BatchFailedResultModel])
batchDeleteResponse_failed = Lens.lens (\BatchDeleteResponse' {failed} -> failed) (\s@BatchDeleteResponse' {} a -> s {failed = a} :: BatchDeleteResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchDeleteResponse_httpStatus :: Lens.Lens' BatchDeleteResponse Prelude.Int
batchDeleteResponse_httpStatus = Lens.lens (\BatchDeleteResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteResponse' {} a -> s {httpStatus = a} :: BatchDeleteResponse)

instance Prelude.NFData BatchDeleteResponse
