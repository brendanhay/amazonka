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
-- Module      : Amazonka.SageMaker.DescribeAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an action.
module Amazonka.SageMaker.DescribeAction
  ( -- * Creating a Request
    DescribeAction (..),
    newDescribeAction,

    -- * Request Lenses
    describeAction_actionName,

    -- * Destructuring the Response
    DescribeActionResponse (..),
    newDescribeActionResponse,

    -- * Response Lenses
    describeActionResponse_actionArn,
    describeActionResponse_actionName,
    describeActionResponse_actionType,
    describeActionResponse_createdBy,
    describeActionResponse_creationTime,
    describeActionResponse_description,
    describeActionResponse_lastModifiedBy,
    describeActionResponse_lastModifiedTime,
    describeActionResponse_lineageGroupArn,
    describeActionResponse_metadataProperties,
    describeActionResponse_properties,
    describeActionResponse_source,
    describeActionResponse_status,
    describeActionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeAction' smart constructor.
data DescribeAction = DescribeAction'
  { -- | The name of the action to describe.
    actionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionName', 'describeAction_actionName' - The name of the action to describe.
newDescribeAction ::
  -- | 'actionName'
  Prelude.Text ->
  DescribeAction
newDescribeAction pActionName_ =
  DescribeAction' {actionName = pActionName_}

-- | The name of the action to describe.
describeAction_actionName :: Lens.Lens' DescribeAction Prelude.Text
describeAction_actionName = Lens.lens (\DescribeAction' {actionName} -> actionName) (\s@DescribeAction' {} a -> s {actionName = a} :: DescribeAction)

instance Core.AWSRequest DescribeAction where
  type
    AWSResponse DescribeAction =
      DescribeActionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeActionResponse'
            Prelude.<$> (x Data..?> "ActionArn")
            Prelude.<*> (x Data..?> "ActionName")
            Prelude.<*> (x Data..?> "ActionType")
            Prelude.<*> (x Data..?> "CreatedBy")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "LastModifiedBy")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "LineageGroupArn")
            Prelude.<*> (x Data..?> "MetadataProperties")
            Prelude.<*> (x Data..?> "Properties" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Source")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAction where
  hashWithSalt _salt DescribeAction' {..} =
    _salt `Prelude.hashWithSalt` actionName

instance Prelude.NFData DescribeAction where
  rnf DescribeAction' {..} = Prelude.rnf actionName

instance Data.ToHeaders DescribeAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DescribeAction" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAction where
  toJSON DescribeAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ActionName" Data..= actionName)]
      )

instance Data.ToPath DescribeAction where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeActionResponse' smart constructor.
data DescribeActionResponse = DescribeActionResponse'
  { -- | The Amazon Resource Name (ARN) of the action.
    actionArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the action.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | The type of the action.
    actionType :: Prelude.Maybe Prelude.Text,
    createdBy :: Prelude.Maybe UserContext,
    -- | When the action was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the action.
    description :: Prelude.Maybe Prelude.Text,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | When the action was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the lineage group.
    lineageGroupArn :: Prelude.Maybe Prelude.Text,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | A list of the action\'s properties.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The source of the action.
    source :: Prelude.Maybe ActionSource,
    -- | The status of the action.
    status :: Prelude.Maybe ActionStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionArn', 'describeActionResponse_actionArn' - The Amazon Resource Name (ARN) of the action.
--
-- 'actionName', 'describeActionResponse_actionName' - The name of the action.
--
-- 'actionType', 'describeActionResponse_actionType' - The type of the action.
--
-- 'createdBy', 'describeActionResponse_createdBy' - Undocumented member.
--
-- 'creationTime', 'describeActionResponse_creationTime' - When the action was created.
--
-- 'description', 'describeActionResponse_description' - The description of the action.
--
-- 'lastModifiedBy', 'describeActionResponse_lastModifiedBy' - Undocumented member.
--
-- 'lastModifiedTime', 'describeActionResponse_lastModifiedTime' - When the action was last modified.
--
-- 'lineageGroupArn', 'describeActionResponse_lineageGroupArn' - The Amazon Resource Name (ARN) of the lineage group.
--
-- 'metadataProperties', 'describeActionResponse_metadataProperties' - Undocumented member.
--
-- 'properties', 'describeActionResponse_properties' - A list of the action\'s properties.
--
-- 'source', 'describeActionResponse_source' - The source of the action.
--
-- 'status', 'describeActionResponse_status' - The status of the action.
--
-- 'httpStatus', 'describeActionResponse_httpStatus' - The response's http status code.
newDescribeActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeActionResponse
newDescribeActionResponse pHttpStatus_ =
  DescribeActionResponse'
    { actionArn =
        Prelude.Nothing,
      actionName = Prelude.Nothing,
      actionType = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lineageGroupArn = Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      properties = Prelude.Nothing,
      source = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the action.
describeActionResponse_actionArn :: Lens.Lens' DescribeActionResponse (Prelude.Maybe Prelude.Text)
describeActionResponse_actionArn = Lens.lens (\DescribeActionResponse' {actionArn} -> actionArn) (\s@DescribeActionResponse' {} a -> s {actionArn = a} :: DescribeActionResponse)

-- | The name of the action.
describeActionResponse_actionName :: Lens.Lens' DescribeActionResponse (Prelude.Maybe Prelude.Text)
describeActionResponse_actionName = Lens.lens (\DescribeActionResponse' {actionName} -> actionName) (\s@DescribeActionResponse' {} a -> s {actionName = a} :: DescribeActionResponse)

-- | The type of the action.
describeActionResponse_actionType :: Lens.Lens' DescribeActionResponse (Prelude.Maybe Prelude.Text)
describeActionResponse_actionType = Lens.lens (\DescribeActionResponse' {actionType} -> actionType) (\s@DescribeActionResponse' {} a -> s {actionType = a} :: DescribeActionResponse)

-- | Undocumented member.
describeActionResponse_createdBy :: Lens.Lens' DescribeActionResponse (Prelude.Maybe UserContext)
describeActionResponse_createdBy = Lens.lens (\DescribeActionResponse' {createdBy} -> createdBy) (\s@DescribeActionResponse' {} a -> s {createdBy = a} :: DescribeActionResponse)

-- | When the action was created.
describeActionResponse_creationTime :: Lens.Lens' DescribeActionResponse (Prelude.Maybe Prelude.UTCTime)
describeActionResponse_creationTime = Lens.lens (\DescribeActionResponse' {creationTime} -> creationTime) (\s@DescribeActionResponse' {} a -> s {creationTime = a} :: DescribeActionResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the action.
describeActionResponse_description :: Lens.Lens' DescribeActionResponse (Prelude.Maybe Prelude.Text)
describeActionResponse_description = Lens.lens (\DescribeActionResponse' {description} -> description) (\s@DescribeActionResponse' {} a -> s {description = a} :: DescribeActionResponse)

-- | Undocumented member.
describeActionResponse_lastModifiedBy :: Lens.Lens' DescribeActionResponse (Prelude.Maybe UserContext)
describeActionResponse_lastModifiedBy = Lens.lens (\DescribeActionResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeActionResponse' {} a -> s {lastModifiedBy = a} :: DescribeActionResponse)

-- | When the action was last modified.
describeActionResponse_lastModifiedTime :: Lens.Lens' DescribeActionResponse (Prelude.Maybe Prelude.UTCTime)
describeActionResponse_lastModifiedTime = Lens.lens (\DescribeActionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeActionResponse' {} a -> s {lastModifiedTime = a} :: DescribeActionResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the lineage group.
describeActionResponse_lineageGroupArn :: Lens.Lens' DescribeActionResponse (Prelude.Maybe Prelude.Text)
describeActionResponse_lineageGroupArn = Lens.lens (\DescribeActionResponse' {lineageGroupArn} -> lineageGroupArn) (\s@DescribeActionResponse' {} a -> s {lineageGroupArn = a} :: DescribeActionResponse)

-- | Undocumented member.
describeActionResponse_metadataProperties :: Lens.Lens' DescribeActionResponse (Prelude.Maybe MetadataProperties)
describeActionResponse_metadataProperties = Lens.lens (\DescribeActionResponse' {metadataProperties} -> metadataProperties) (\s@DescribeActionResponse' {} a -> s {metadataProperties = a} :: DescribeActionResponse)

-- | A list of the action\'s properties.
describeActionResponse_properties :: Lens.Lens' DescribeActionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeActionResponse_properties = Lens.lens (\DescribeActionResponse' {properties} -> properties) (\s@DescribeActionResponse' {} a -> s {properties = a} :: DescribeActionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The source of the action.
describeActionResponse_source :: Lens.Lens' DescribeActionResponse (Prelude.Maybe ActionSource)
describeActionResponse_source = Lens.lens (\DescribeActionResponse' {source} -> source) (\s@DescribeActionResponse' {} a -> s {source = a} :: DescribeActionResponse)

-- | The status of the action.
describeActionResponse_status :: Lens.Lens' DescribeActionResponse (Prelude.Maybe ActionStatus)
describeActionResponse_status = Lens.lens (\DescribeActionResponse' {status} -> status) (\s@DescribeActionResponse' {} a -> s {status = a} :: DescribeActionResponse)

-- | The response's http status code.
describeActionResponse_httpStatus :: Lens.Lens' DescribeActionResponse Prelude.Int
describeActionResponse_httpStatus = Lens.lens (\DescribeActionResponse' {httpStatus} -> httpStatus) (\s@DescribeActionResponse' {} a -> s {httpStatus = a} :: DescribeActionResponse)

instance Prelude.NFData DescribeActionResponse where
  rnf DescribeActionResponse' {..} =
    Prelude.rnf actionArn
      `Prelude.seq` Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf actionType
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf lineageGroupArn
      `Prelude.seq` Prelude.rnf metadataProperties
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
