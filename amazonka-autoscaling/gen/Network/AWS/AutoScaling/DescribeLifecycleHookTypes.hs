{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHookTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available types of lifecycle hooks.
--
-- The following hook types are supported:
--
-- -   autoscaling:EC2_INSTANCE_LAUNCHING
--
-- -   autoscaling:EC2_INSTANCE_TERMINATING
module Network.AWS.AutoScaling.DescribeLifecycleHookTypes
  ( -- * Creating a Request
    DescribeLifecycleHookTypes (..),
    newDescribeLifecycleHookTypes,

    -- * Destructuring the Response
    DescribeLifecycleHookTypesResponse (..),
    newDescribeLifecycleHookTypesResponse,

    -- * Response Lenses
    describeLifecycleHookTypesResponse_lifecycleHookTypes,
    describeLifecycleHookTypesResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLifecycleHookTypes' smart constructor.
data DescribeLifecycleHookTypes = DescribeLifecycleHookTypes'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeLifecycleHookTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeLifecycleHookTypes ::
  DescribeLifecycleHookTypes
newDescribeLifecycleHookTypes =
  DescribeLifecycleHookTypes'

instance
  Prelude.AWSRequest
    DescribeLifecycleHookTypes
  where
  type
    Rs DescribeLifecycleHookTypes =
      DescribeLifecycleHookTypesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeLifecycleHookTypesResult"
      ( \s h x ->
          DescribeLifecycleHookTypesResponse'
            Prelude.<$> ( x Prelude..@? "LifecycleHookTypes"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLifecycleHookTypes

instance Prelude.NFData DescribeLifecycleHookTypes

instance Prelude.ToHeaders DescribeLifecycleHookTypes where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeLifecycleHookTypes where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeLifecycleHookTypes where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Prelude.=: ("DescribeLifecycleHookTypes" :: Prelude.ByteString),
            "Version"
              Prelude.=: ("2011-01-01" :: Prelude.ByteString)
          ]
      )

-- | /See:/ 'newDescribeLifecycleHookTypesResponse' smart constructor.
data DescribeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse'
  { -- | The lifecycle hook types.
    lifecycleHookTypes :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeLifecycleHookTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycleHookTypes', 'describeLifecycleHookTypesResponse_lifecycleHookTypes' - The lifecycle hook types.
--
-- 'httpStatus', 'describeLifecycleHookTypesResponse_httpStatus' - The response's http status code.
newDescribeLifecycleHookTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLifecycleHookTypesResponse
newDescribeLifecycleHookTypesResponse pHttpStatus_ =
  DescribeLifecycleHookTypesResponse'
    { lifecycleHookTypes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The lifecycle hook types.
describeLifecycleHookTypesResponse_lifecycleHookTypes :: Lens.Lens' DescribeLifecycleHookTypesResponse (Prelude.Maybe [Prelude.Text])
describeLifecycleHookTypesResponse_lifecycleHookTypes = Lens.lens (\DescribeLifecycleHookTypesResponse' {lifecycleHookTypes} -> lifecycleHookTypes) (\s@DescribeLifecycleHookTypesResponse' {} a -> s {lifecycleHookTypes = a} :: DescribeLifecycleHookTypesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeLifecycleHookTypesResponse_httpStatus :: Lens.Lens' DescribeLifecycleHookTypesResponse Prelude.Int
describeLifecycleHookTypesResponse_httpStatus = Lens.lens (\DescribeLifecycleHookTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeLifecycleHookTypesResponse' {} a -> s {httpStatus = a} :: DescribeLifecycleHookTypesResponse)

instance
  Prelude.NFData
    DescribeLifecycleHookTypesResponse
