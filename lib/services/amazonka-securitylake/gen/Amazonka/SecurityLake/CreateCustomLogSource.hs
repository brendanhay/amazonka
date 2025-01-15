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
-- Module      : Amazonka.SecurityLake.CreateCustomLogSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a third-party custom source in Amazon Security Lake, from the
-- Amazon Web Services Region where you want to create a custom source.
-- Security Lake can collect logs and events from third-party custom
-- sources. After creating the appropriate IAM role to invoke Glue crawler,
-- use this API to add a custom source name in Security Lake. This
-- operation creates a partition in the Amazon S3 bucket for Security Lake
-- as the target location for log files from the custom source in addition
-- to an associated Glue table and an Glue crawler.
module Amazonka.SecurityLake.CreateCustomLogSource
  ( -- * Creating a Request
    CreateCustomLogSource (..),
    newCreateCustomLogSource,

    -- * Request Lenses
    createCustomLogSource_customSourceName,
    createCustomLogSource_eventClass,
    createCustomLogSource_glueInvocationRoleArn,
    createCustomLogSource_logProviderAccountId,

    -- * Destructuring the Response
    CreateCustomLogSourceResponse (..),
    newCreateCustomLogSourceResponse,

    -- * Response Lenses
    createCustomLogSourceResponse_httpStatus,
    createCustomLogSourceResponse_customDataLocation,
    createCustomLogSourceResponse_glueCrawlerName,
    createCustomLogSourceResponse_glueDatabaseName,
    createCustomLogSourceResponse_glueTableName,
    createCustomLogSourceResponse_logProviderAccessRoleArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newCreateCustomLogSource' smart constructor.
data CreateCustomLogSource = CreateCustomLogSource'
  { -- | The name for a third-party custom source. This must be a Regionally
    -- unique value.
    customSourceName :: Prelude.Text,
    -- | The Open Cybersecurity Schema Framework (OCSF) event class which
    -- describes the type of data that the custom source will send to Security
    -- Lake.
    eventClass :: OcsfEventClass,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role to be used by the Glue crawler. The recommended IAM policies
    -- are:
    --
    -- -   The managed policy @AWSGlueServiceRole@
    --
    -- -   A custom policy granting access to your Amazon S3 Data Lake
    glueInvocationRoleArn :: Prelude.Text,
    -- | The Amazon Web Services account ID of the custom source that will write
    -- logs and events into the Amazon S3 Data Lake.
    logProviderAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomLogSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customSourceName', 'createCustomLogSource_customSourceName' - The name for a third-party custom source. This must be a Regionally
-- unique value.
--
-- 'eventClass', 'createCustomLogSource_eventClass' - The Open Cybersecurity Schema Framework (OCSF) event class which
-- describes the type of data that the custom source will send to Security
-- Lake.
--
-- 'glueInvocationRoleArn', 'createCustomLogSource_glueInvocationRoleArn' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role to be used by the Glue crawler. The recommended IAM policies
-- are:
--
-- -   The managed policy @AWSGlueServiceRole@
--
-- -   A custom policy granting access to your Amazon S3 Data Lake
--
-- 'logProviderAccountId', 'createCustomLogSource_logProviderAccountId' - The Amazon Web Services account ID of the custom source that will write
-- logs and events into the Amazon S3 Data Lake.
newCreateCustomLogSource ::
  -- | 'customSourceName'
  Prelude.Text ->
  -- | 'eventClass'
  OcsfEventClass ->
  -- | 'glueInvocationRoleArn'
  Prelude.Text ->
  -- | 'logProviderAccountId'
  Prelude.Text ->
  CreateCustomLogSource
newCreateCustomLogSource
  pCustomSourceName_
  pEventClass_
  pGlueInvocationRoleArn_
  pLogProviderAccountId_ =
    CreateCustomLogSource'
      { customSourceName =
          pCustomSourceName_,
        eventClass = pEventClass_,
        glueInvocationRoleArn = pGlueInvocationRoleArn_,
        logProviderAccountId = pLogProviderAccountId_
      }

-- | The name for a third-party custom source. This must be a Regionally
-- unique value.
createCustomLogSource_customSourceName :: Lens.Lens' CreateCustomLogSource Prelude.Text
createCustomLogSource_customSourceName = Lens.lens (\CreateCustomLogSource' {customSourceName} -> customSourceName) (\s@CreateCustomLogSource' {} a -> s {customSourceName = a} :: CreateCustomLogSource)

-- | The Open Cybersecurity Schema Framework (OCSF) event class which
-- describes the type of data that the custom source will send to Security
-- Lake.
createCustomLogSource_eventClass :: Lens.Lens' CreateCustomLogSource OcsfEventClass
createCustomLogSource_eventClass = Lens.lens (\CreateCustomLogSource' {eventClass} -> eventClass) (\s@CreateCustomLogSource' {} a -> s {eventClass = a} :: CreateCustomLogSource)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role to be used by the Glue crawler. The recommended IAM policies
-- are:
--
-- -   The managed policy @AWSGlueServiceRole@
--
-- -   A custom policy granting access to your Amazon S3 Data Lake
createCustomLogSource_glueInvocationRoleArn :: Lens.Lens' CreateCustomLogSource Prelude.Text
createCustomLogSource_glueInvocationRoleArn = Lens.lens (\CreateCustomLogSource' {glueInvocationRoleArn} -> glueInvocationRoleArn) (\s@CreateCustomLogSource' {} a -> s {glueInvocationRoleArn = a} :: CreateCustomLogSource)

-- | The Amazon Web Services account ID of the custom source that will write
-- logs and events into the Amazon S3 Data Lake.
createCustomLogSource_logProviderAccountId :: Lens.Lens' CreateCustomLogSource Prelude.Text
createCustomLogSource_logProviderAccountId = Lens.lens (\CreateCustomLogSource' {logProviderAccountId} -> logProviderAccountId) (\s@CreateCustomLogSource' {} a -> s {logProviderAccountId = a} :: CreateCustomLogSource)

instance Core.AWSRequest CreateCustomLogSource where
  type
    AWSResponse CreateCustomLogSource =
      CreateCustomLogSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomLogSourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "customDataLocation")
            Prelude.<*> (x Data..:> "glueCrawlerName")
            Prelude.<*> (x Data..:> "glueDatabaseName")
            Prelude.<*> (x Data..:> "glueTableName")
            Prelude.<*> (x Data..:> "logProviderAccessRoleArn")
      )

instance Prelude.Hashable CreateCustomLogSource where
  hashWithSalt _salt CreateCustomLogSource' {..} =
    _salt
      `Prelude.hashWithSalt` customSourceName
      `Prelude.hashWithSalt` eventClass
      `Prelude.hashWithSalt` glueInvocationRoleArn
      `Prelude.hashWithSalt` logProviderAccountId

instance Prelude.NFData CreateCustomLogSource where
  rnf CreateCustomLogSource' {..} =
    Prelude.rnf customSourceName `Prelude.seq`
      Prelude.rnf eventClass `Prelude.seq`
        Prelude.rnf glueInvocationRoleArn `Prelude.seq`
          Prelude.rnf logProviderAccountId

instance Data.ToHeaders CreateCustomLogSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCustomLogSource where
  toJSON CreateCustomLogSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("customSourceName" Data..= customSourceName),
            Prelude.Just ("eventClass" Data..= eventClass),
            Prelude.Just
              ( "glueInvocationRoleArn"
                  Data..= glueInvocationRoleArn
              ),
            Prelude.Just
              ( "logProviderAccountId"
                  Data..= logProviderAccountId
              )
          ]
      )

instance Data.ToPath CreateCustomLogSource where
  toPath = Prelude.const "/v1/logsources/custom"

instance Data.ToQuery CreateCustomLogSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCustomLogSourceResponse' smart constructor.
data CreateCustomLogSourceResponse = CreateCustomLogSourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The location of the partition in the Amazon S3 bucket for Security Lake.
    customDataLocation :: Prelude.Text,
    -- | The name of the Glue crawler.
    glueCrawlerName :: Prelude.Text,
    -- | The Glue database where results are written, such as:
    -- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
    glueDatabaseName :: Prelude.Text,
    -- | The table name of the Glue crawler.
    glueTableName :: Prelude.Text,
    -- | The ARN of the IAM role to be used by the entity putting logs into your
    -- custom source partition. Security Lake will apply the correct access
    -- policies to this role, but you must first manually create the trust
    -- policy for this role. The IAM role name must start with the text
    -- \'Security Lake\'. The IAM role must trust the @logProviderAccountId@ to
    -- assume the role.
    logProviderAccessRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomLogSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createCustomLogSourceResponse_httpStatus' - The response's http status code.
--
-- 'customDataLocation', 'createCustomLogSourceResponse_customDataLocation' - The location of the partition in the Amazon S3 bucket for Security Lake.
--
-- 'glueCrawlerName', 'createCustomLogSourceResponse_glueCrawlerName' - The name of the Glue crawler.
--
-- 'glueDatabaseName', 'createCustomLogSourceResponse_glueDatabaseName' - The Glue database where results are written, such as:
-- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
--
-- 'glueTableName', 'createCustomLogSourceResponse_glueTableName' - The table name of the Glue crawler.
--
-- 'logProviderAccessRoleArn', 'createCustomLogSourceResponse_logProviderAccessRoleArn' - The ARN of the IAM role to be used by the entity putting logs into your
-- custom source partition. Security Lake will apply the correct access
-- policies to this role, but you must first manually create the trust
-- policy for this role. The IAM role name must start with the text
-- \'Security Lake\'. The IAM role must trust the @logProviderAccountId@ to
-- assume the role.
newCreateCustomLogSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'customDataLocation'
  Prelude.Text ->
  -- | 'glueCrawlerName'
  Prelude.Text ->
  -- | 'glueDatabaseName'
  Prelude.Text ->
  -- | 'glueTableName'
  Prelude.Text ->
  -- | 'logProviderAccessRoleArn'
  Prelude.Text ->
  CreateCustomLogSourceResponse
newCreateCustomLogSourceResponse
  pHttpStatus_
  pCustomDataLocation_
  pGlueCrawlerName_
  pGlueDatabaseName_
  pGlueTableName_
  pLogProviderAccessRoleArn_ =
    CreateCustomLogSourceResponse'
      { httpStatus =
          pHttpStatus_,
        customDataLocation = pCustomDataLocation_,
        glueCrawlerName = pGlueCrawlerName_,
        glueDatabaseName = pGlueDatabaseName_,
        glueTableName = pGlueTableName_,
        logProviderAccessRoleArn =
          pLogProviderAccessRoleArn_
      }

-- | The response's http status code.
createCustomLogSourceResponse_httpStatus :: Lens.Lens' CreateCustomLogSourceResponse Prelude.Int
createCustomLogSourceResponse_httpStatus = Lens.lens (\CreateCustomLogSourceResponse' {httpStatus} -> httpStatus) (\s@CreateCustomLogSourceResponse' {} a -> s {httpStatus = a} :: CreateCustomLogSourceResponse)

-- | The location of the partition in the Amazon S3 bucket for Security Lake.
createCustomLogSourceResponse_customDataLocation :: Lens.Lens' CreateCustomLogSourceResponse Prelude.Text
createCustomLogSourceResponse_customDataLocation = Lens.lens (\CreateCustomLogSourceResponse' {customDataLocation} -> customDataLocation) (\s@CreateCustomLogSourceResponse' {} a -> s {customDataLocation = a} :: CreateCustomLogSourceResponse)

-- | The name of the Glue crawler.
createCustomLogSourceResponse_glueCrawlerName :: Lens.Lens' CreateCustomLogSourceResponse Prelude.Text
createCustomLogSourceResponse_glueCrawlerName = Lens.lens (\CreateCustomLogSourceResponse' {glueCrawlerName} -> glueCrawlerName) (\s@CreateCustomLogSourceResponse' {} a -> s {glueCrawlerName = a} :: CreateCustomLogSourceResponse)

-- | The Glue database where results are written, such as:
-- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
createCustomLogSourceResponse_glueDatabaseName :: Lens.Lens' CreateCustomLogSourceResponse Prelude.Text
createCustomLogSourceResponse_glueDatabaseName = Lens.lens (\CreateCustomLogSourceResponse' {glueDatabaseName} -> glueDatabaseName) (\s@CreateCustomLogSourceResponse' {} a -> s {glueDatabaseName = a} :: CreateCustomLogSourceResponse)

-- | The table name of the Glue crawler.
createCustomLogSourceResponse_glueTableName :: Lens.Lens' CreateCustomLogSourceResponse Prelude.Text
createCustomLogSourceResponse_glueTableName = Lens.lens (\CreateCustomLogSourceResponse' {glueTableName} -> glueTableName) (\s@CreateCustomLogSourceResponse' {} a -> s {glueTableName = a} :: CreateCustomLogSourceResponse)

-- | The ARN of the IAM role to be used by the entity putting logs into your
-- custom source partition. Security Lake will apply the correct access
-- policies to this role, but you must first manually create the trust
-- policy for this role. The IAM role name must start with the text
-- \'Security Lake\'. The IAM role must trust the @logProviderAccountId@ to
-- assume the role.
createCustomLogSourceResponse_logProviderAccessRoleArn :: Lens.Lens' CreateCustomLogSourceResponse Prelude.Text
createCustomLogSourceResponse_logProviderAccessRoleArn = Lens.lens (\CreateCustomLogSourceResponse' {logProviderAccessRoleArn} -> logProviderAccessRoleArn) (\s@CreateCustomLogSourceResponse' {} a -> s {logProviderAccessRoleArn = a} :: CreateCustomLogSourceResponse)

instance Prelude.NFData CreateCustomLogSourceResponse where
  rnf CreateCustomLogSourceResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf customDataLocation `Prelude.seq`
        Prelude.rnf glueCrawlerName `Prelude.seq`
          Prelude.rnf glueDatabaseName `Prelude.seq`
            Prelude.rnf glueTableName `Prelude.seq`
              Prelude.rnf logProviderAccessRoleArn
