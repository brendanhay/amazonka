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
-- Module      : Network.AWS.StepFunctions.DescribeActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an activity.
--
-- This operation is eventually consistent. The results are best effort and
-- may not reflect very recent updates and changes.
module Network.AWS.StepFunctions.DescribeActivity
  ( -- * Creating a Request
    DescribeActivity (..),
    newDescribeActivity,

    -- * Request Lenses
    describeActivity_activityArn,

    -- * Destructuring the Response
    DescribeActivityResponse (..),
    newDescribeActivityResponse,

    -- * Response Lenses
    describeActivityResponse_httpStatus,
    describeActivityResponse_activityArn,
    describeActivityResponse_name,
    describeActivityResponse_creationDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newDescribeActivity' smart constructor.
data DescribeActivity = DescribeActivity'
  { -- | The Amazon Resource Name (ARN) of the activity to describe.
    activityArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activityArn', 'describeActivity_activityArn' - The Amazon Resource Name (ARN) of the activity to describe.
newDescribeActivity ::
  -- | 'activityArn'
  Prelude.Text ->
  DescribeActivity
newDescribeActivity pActivityArn_ =
  DescribeActivity' {activityArn = pActivityArn_}

-- | The Amazon Resource Name (ARN) of the activity to describe.
describeActivity_activityArn :: Lens.Lens' DescribeActivity Prelude.Text
describeActivity_activityArn = Lens.lens (\DescribeActivity' {activityArn} -> activityArn) (\s@DescribeActivity' {} a -> s {activityArn = a} :: DescribeActivity)

instance Prelude.AWSRequest DescribeActivity where
  type Rs DescribeActivity = DescribeActivityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeActivityResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "activityArn")
            Prelude.<*> (x Prelude..:> "name")
            Prelude.<*> (x Prelude..:> "creationDate")
      )

instance Prelude.Hashable DescribeActivity

instance Prelude.NFData DescribeActivity

instance Prelude.ToHeaders DescribeActivity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSStepFunctions.DescribeActivity" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeActivity where
  toJSON DescribeActivity' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("activityArn" Prelude..= activityArn)
          ]
      )

instance Prelude.ToPath DescribeActivity where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeActivity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeActivityResponse' smart constructor.
data DescribeActivityResponse = DescribeActivityResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) that identifies the activity.
    activityArn :: Prelude.Text,
    -- | The name of the activity.
    --
    -- A name must /not/ contain:
    --
    -- -   white space
    --
    -- -   brackets @\< > { } [ ]@
    --
    -- -   wildcard characters @? *@
    --
    -- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
    --
    -- -   control characters (@U+0000-001F@, @U+007F-009F@)
    --
    -- To enable logging with CloudWatch Logs, the name should only contain
    -- 0-9, A-Z, a-z, - and _.
    name :: Prelude.Text,
    -- | The date the activity is created.
    creationDate :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeActivityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeActivityResponse_httpStatus' - The response's http status code.
--
-- 'activityArn', 'describeActivityResponse_activityArn' - The Amazon Resource Name (ARN) that identifies the activity.
--
-- 'name', 'describeActivityResponse_name' - The name of the activity.
--
-- A name must /not/ contain:
--
-- -   white space
--
-- -   brackets @\< > { } [ ]@
--
-- -   wildcard characters @? *@
--
-- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
--
-- -   control characters (@U+0000-001F@, @U+007F-009F@)
--
-- To enable logging with CloudWatch Logs, the name should only contain
-- 0-9, A-Z, a-z, - and _.
--
-- 'creationDate', 'describeActivityResponse_creationDate' - The date the activity is created.
newDescribeActivityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'activityArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.UTCTime ->
  DescribeActivityResponse
newDescribeActivityResponse
  pHttpStatus_
  pActivityArn_
  pName_
  pCreationDate_ =
    DescribeActivityResponse'
      { httpStatus =
          pHttpStatus_,
        activityArn = pActivityArn_,
        name = pName_,
        creationDate =
          Prelude._Time Lens.# pCreationDate_
      }

-- | The response's http status code.
describeActivityResponse_httpStatus :: Lens.Lens' DescribeActivityResponse Prelude.Int
describeActivityResponse_httpStatus = Lens.lens (\DescribeActivityResponse' {httpStatus} -> httpStatus) (\s@DescribeActivityResponse' {} a -> s {httpStatus = a} :: DescribeActivityResponse)

-- | The Amazon Resource Name (ARN) that identifies the activity.
describeActivityResponse_activityArn :: Lens.Lens' DescribeActivityResponse Prelude.Text
describeActivityResponse_activityArn = Lens.lens (\DescribeActivityResponse' {activityArn} -> activityArn) (\s@DescribeActivityResponse' {} a -> s {activityArn = a} :: DescribeActivityResponse)

-- | The name of the activity.
--
-- A name must /not/ contain:
--
-- -   white space
--
-- -   brackets @\< > { } [ ]@
--
-- -   wildcard characters @? *@
--
-- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
--
-- -   control characters (@U+0000-001F@, @U+007F-009F@)
--
-- To enable logging with CloudWatch Logs, the name should only contain
-- 0-9, A-Z, a-z, - and _.
describeActivityResponse_name :: Lens.Lens' DescribeActivityResponse Prelude.Text
describeActivityResponse_name = Lens.lens (\DescribeActivityResponse' {name} -> name) (\s@DescribeActivityResponse' {} a -> s {name = a} :: DescribeActivityResponse)

-- | The date the activity is created.
describeActivityResponse_creationDate :: Lens.Lens' DescribeActivityResponse Prelude.UTCTime
describeActivityResponse_creationDate = Lens.lens (\DescribeActivityResponse' {creationDate} -> creationDate) (\s@DescribeActivityResponse' {} a -> s {creationDate = a} :: DescribeActivityResponse) Prelude.. Prelude._Time

instance Prelude.NFData DescribeActivityResponse
