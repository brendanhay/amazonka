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
-- Module      : Amazonka.GlobalAccelerator.DescribeCustomRoutingListener
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The description of a listener for a custom routing accelerator.
module Amazonka.GlobalAccelerator.DescribeCustomRoutingListener
  ( -- * Creating a Request
    DescribeCustomRoutingListener (..),
    newDescribeCustomRoutingListener,

    -- * Request Lenses
    describeCustomRoutingListener_listenerArn,

    -- * Destructuring the Response
    DescribeCustomRoutingListenerResponse (..),
    newDescribeCustomRoutingListenerResponse,

    -- * Response Lenses
    describeCustomRoutingListenerResponse_listener,
    describeCustomRoutingListenerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCustomRoutingListener' smart constructor.
data DescribeCustomRoutingListener = DescribeCustomRoutingListener'
  { -- | The Amazon Resource Name (ARN) of the listener to describe.
    listenerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomRoutingListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listenerArn', 'describeCustomRoutingListener_listenerArn' - The Amazon Resource Name (ARN) of the listener to describe.
newDescribeCustomRoutingListener ::
  -- | 'listenerArn'
  Prelude.Text ->
  DescribeCustomRoutingListener
newDescribeCustomRoutingListener pListenerArn_ =
  DescribeCustomRoutingListener'
    { listenerArn =
        pListenerArn_
    }

-- | The Amazon Resource Name (ARN) of the listener to describe.
describeCustomRoutingListener_listenerArn :: Lens.Lens' DescribeCustomRoutingListener Prelude.Text
describeCustomRoutingListener_listenerArn = Lens.lens (\DescribeCustomRoutingListener' {listenerArn} -> listenerArn) (\s@DescribeCustomRoutingListener' {} a -> s {listenerArn = a} :: DescribeCustomRoutingListener)

instance
  Core.AWSRequest
    DescribeCustomRoutingListener
  where
  type
    AWSResponse DescribeCustomRoutingListener =
      DescribeCustomRoutingListenerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCustomRoutingListenerResponse'
            Prelude.<$> (x Data..?> "Listener")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeCustomRoutingListener
  where
  hashWithSalt _salt DescribeCustomRoutingListener' {..} =
    _salt `Prelude.hashWithSalt` listenerArn

instance Prelude.NFData DescribeCustomRoutingListener where
  rnf DescribeCustomRoutingListener' {..} =
    Prelude.rnf listenerArn

instance Data.ToHeaders DescribeCustomRoutingListener where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.DescribeCustomRoutingListener" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeCustomRoutingListener where
  toJSON DescribeCustomRoutingListener' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ListenerArn" Data..= listenerArn)]
      )

instance Data.ToPath DescribeCustomRoutingListener where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCustomRoutingListener where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCustomRoutingListenerResponse' smart constructor.
data DescribeCustomRoutingListenerResponse = DescribeCustomRoutingListenerResponse'
  { -- | The description of a listener for a custom routing accelerator.
    listener :: Prelude.Maybe CustomRoutingListener,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomRoutingListenerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listener', 'describeCustomRoutingListenerResponse_listener' - The description of a listener for a custom routing accelerator.
--
-- 'httpStatus', 'describeCustomRoutingListenerResponse_httpStatus' - The response's http status code.
newDescribeCustomRoutingListenerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCustomRoutingListenerResponse
newDescribeCustomRoutingListenerResponse pHttpStatus_ =
  DescribeCustomRoutingListenerResponse'
    { listener =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of a listener for a custom routing accelerator.
describeCustomRoutingListenerResponse_listener :: Lens.Lens' DescribeCustomRoutingListenerResponse (Prelude.Maybe CustomRoutingListener)
describeCustomRoutingListenerResponse_listener = Lens.lens (\DescribeCustomRoutingListenerResponse' {listener} -> listener) (\s@DescribeCustomRoutingListenerResponse' {} a -> s {listener = a} :: DescribeCustomRoutingListenerResponse)

-- | The response's http status code.
describeCustomRoutingListenerResponse_httpStatus :: Lens.Lens' DescribeCustomRoutingListenerResponse Prelude.Int
describeCustomRoutingListenerResponse_httpStatus = Lens.lens (\DescribeCustomRoutingListenerResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomRoutingListenerResponse' {} a -> s {httpStatus = a} :: DescribeCustomRoutingListenerResponse)

instance
  Prelude.NFData
    DescribeCustomRoutingListenerResponse
  where
  rnf DescribeCustomRoutingListenerResponse' {..} =
    Prelude.rnf listener `Prelude.seq`
      Prelude.rnf httpStatus
