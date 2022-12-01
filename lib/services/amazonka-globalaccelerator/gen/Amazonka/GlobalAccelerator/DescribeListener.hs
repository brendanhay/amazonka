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
-- Module      : Amazonka.GlobalAccelerator.DescribeListener
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe a listener.
module Amazonka.GlobalAccelerator.DescribeListener
  ( -- * Creating a Request
    DescribeListener (..),
    newDescribeListener,

    -- * Request Lenses
    describeListener_listenerArn,

    -- * Destructuring the Response
    DescribeListenerResponse (..),
    newDescribeListenerResponse,

    -- * Response Lenses
    describeListenerResponse_listener,
    describeListenerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeListener' smart constructor.
data DescribeListener = DescribeListener'
  { -- | The Amazon Resource Name (ARN) of the listener to describe.
    listenerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listenerArn', 'describeListener_listenerArn' - The Amazon Resource Name (ARN) of the listener to describe.
newDescribeListener ::
  -- | 'listenerArn'
  Prelude.Text ->
  DescribeListener
newDescribeListener pListenerArn_ =
  DescribeListener' {listenerArn = pListenerArn_}

-- | The Amazon Resource Name (ARN) of the listener to describe.
describeListener_listenerArn :: Lens.Lens' DescribeListener Prelude.Text
describeListener_listenerArn = Lens.lens (\DescribeListener' {listenerArn} -> listenerArn) (\s@DescribeListener' {} a -> s {listenerArn = a} :: DescribeListener)

instance Core.AWSRequest DescribeListener where
  type
    AWSResponse DescribeListener =
      DescribeListenerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeListenerResponse'
            Prelude.<$> (x Core..?> "Listener")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeListener where
  hashWithSalt _salt DescribeListener' {..} =
    _salt `Prelude.hashWithSalt` listenerArn

instance Prelude.NFData DescribeListener where
  rnf DescribeListener' {..} = Prelude.rnf listenerArn

instance Core.ToHeaders DescribeListener where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.DescribeListener" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeListener where
  toJSON DescribeListener' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ListenerArn" Core..= listenerArn)]
      )

instance Core.ToPath DescribeListener where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeListener where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeListenerResponse' smart constructor.
data DescribeListenerResponse = DescribeListenerResponse'
  { -- | The description of a listener.
    listener :: Prelude.Maybe Listener,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeListenerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listener', 'describeListenerResponse_listener' - The description of a listener.
--
-- 'httpStatus', 'describeListenerResponse_httpStatus' - The response's http status code.
newDescribeListenerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeListenerResponse
newDescribeListenerResponse pHttpStatus_ =
  DescribeListenerResponse'
    { listener =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of a listener.
describeListenerResponse_listener :: Lens.Lens' DescribeListenerResponse (Prelude.Maybe Listener)
describeListenerResponse_listener = Lens.lens (\DescribeListenerResponse' {listener} -> listener) (\s@DescribeListenerResponse' {} a -> s {listener = a} :: DescribeListenerResponse)

-- | The response's http status code.
describeListenerResponse_httpStatus :: Lens.Lens' DescribeListenerResponse Prelude.Int
describeListenerResponse_httpStatus = Lens.lens (\DescribeListenerResponse' {httpStatus} -> httpStatus) (\s@DescribeListenerResponse' {} a -> s {httpStatus = a} :: DescribeListenerResponse)

instance Prelude.NFData DescribeListenerResponse where
  rnf DescribeListenerResponse' {..} =
    Prelude.rnf listener
      `Prelude.seq` Prelude.rnf httpStatus
