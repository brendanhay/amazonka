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
-- Module      : Amazonka.KafkaConnect.DescribeCustomPlugin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A summary description of the custom plugin.
module Amazonka.KafkaConnect.DescribeCustomPlugin
  ( -- * Creating a Request
    DescribeCustomPlugin (..),
    newDescribeCustomPlugin,

    -- * Request Lenses
    describeCustomPlugin_customPluginArn,

    -- * Destructuring the Response
    DescribeCustomPluginResponse (..),
    newDescribeCustomPluginResponse,

    -- * Response Lenses
    describeCustomPluginResponse_creationTime,
    describeCustomPluginResponse_customPluginArn,
    describeCustomPluginResponse_customPluginState,
    describeCustomPluginResponse_description,
    describeCustomPluginResponse_latestRevision,
    describeCustomPluginResponse_name,
    describeCustomPluginResponse_stateDescription,
    describeCustomPluginResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCustomPlugin' smart constructor.
data DescribeCustomPlugin = DescribeCustomPlugin'
  { -- | Returns information about a custom plugin.
    customPluginArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomPlugin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customPluginArn', 'describeCustomPlugin_customPluginArn' - Returns information about a custom plugin.
newDescribeCustomPlugin ::
  -- | 'customPluginArn'
  Prelude.Text ->
  DescribeCustomPlugin
newDescribeCustomPlugin pCustomPluginArn_ =
  DescribeCustomPlugin'
    { customPluginArn =
        pCustomPluginArn_
    }

-- | Returns information about a custom plugin.
describeCustomPlugin_customPluginArn :: Lens.Lens' DescribeCustomPlugin Prelude.Text
describeCustomPlugin_customPluginArn = Lens.lens (\DescribeCustomPlugin' {customPluginArn} -> customPluginArn) (\s@DescribeCustomPlugin' {} a -> s {customPluginArn = a} :: DescribeCustomPlugin)

instance Core.AWSRequest DescribeCustomPlugin where
  type
    AWSResponse DescribeCustomPlugin =
      DescribeCustomPluginResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCustomPluginResponse'
            Prelude.<$> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "customPluginArn")
            Prelude.<*> (x Data..?> "customPluginState")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "latestRevision")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "stateDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCustomPlugin where
  hashWithSalt _salt DescribeCustomPlugin' {..} =
    _salt `Prelude.hashWithSalt` customPluginArn

instance Prelude.NFData DescribeCustomPlugin where
  rnf DescribeCustomPlugin' {..} =
    Prelude.rnf customPluginArn

instance Data.ToHeaders DescribeCustomPlugin where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeCustomPlugin where
  toPath DescribeCustomPlugin' {..} =
    Prelude.mconcat
      ["/v1/custom-plugins/", Data.toBS customPluginArn]

instance Data.ToQuery DescribeCustomPlugin where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCustomPluginResponse' smart constructor.
data DescribeCustomPluginResponse = DescribeCustomPluginResponse'
  { -- | The time that the custom plugin was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Resource Name (ARN) of the custom plugin.
    customPluginArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the custom plugin.
    customPluginState :: Prelude.Maybe CustomPluginState,
    -- | The description of the custom plugin.
    description :: Prelude.Maybe Prelude.Text,
    -- | The latest successfully created revision of the custom plugin. If there
    -- are no successfully created revisions, this field will be absent.
    latestRevision :: Prelude.Maybe CustomPluginRevisionSummary,
    -- | The name of the custom plugin.
    name :: Prelude.Maybe Prelude.Text,
    -- | Details about the state of a custom plugin.
    stateDescription :: Prelude.Maybe StateDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomPluginResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeCustomPluginResponse_creationTime' - The time that the custom plugin was created.
--
-- 'customPluginArn', 'describeCustomPluginResponse_customPluginArn' - The Amazon Resource Name (ARN) of the custom plugin.
--
-- 'customPluginState', 'describeCustomPluginResponse_customPluginState' - The state of the custom plugin.
--
-- 'description', 'describeCustomPluginResponse_description' - The description of the custom plugin.
--
-- 'latestRevision', 'describeCustomPluginResponse_latestRevision' - The latest successfully created revision of the custom plugin. If there
-- are no successfully created revisions, this field will be absent.
--
-- 'name', 'describeCustomPluginResponse_name' - The name of the custom plugin.
--
-- 'stateDescription', 'describeCustomPluginResponse_stateDescription' - Details about the state of a custom plugin.
--
-- 'httpStatus', 'describeCustomPluginResponse_httpStatus' - The response's http status code.
newDescribeCustomPluginResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCustomPluginResponse
newDescribeCustomPluginResponse pHttpStatus_ =
  DescribeCustomPluginResponse'
    { creationTime =
        Prelude.Nothing,
      customPluginArn = Prelude.Nothing,
      customPluginState = Prelude.Nothing,
      description = Prelude.Nothing,
      latestRevision = Prelude.Nothing,
      name = Prelude.Nothing,
      stateDescription = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time that the custom plugin was created.
describeCustomPluginResponse_creationTime :: Lens.Lens' DescribeCustomPluginResponse (Prelude.Maybe Prelude.UTCTime)
describeCustomPluginResponse_creationTime = Lens.lens (\DescribeCustomPluginResponse' {creationTime} -> creationTime) (\s@DescribeCustomPluginResponse' {} a -> s {creationTime = a} :: DescribeCustomPluginResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the custom plugin.
describeCustomPluginResponse_customPluginArn :: Lens.Lens' DescribeCustomPluginResponse (Prelude.Maybe Prelude.Text)
describeCustomPluginResponse_customPluginArn = Lens.lens (\DescribeCustomPluginResponse' {customPluginArn} -> customPluginArn) (\s@DescribeCustomPluginResponse' {} a -> s {customPluginArn = a} :: DescribeCustomPluginResponse)

-- | The state of the custom plugin.
describeCustomPluginResponse_customPluginState :: Lens.Lens' DescribeCustomPluginResponse (Prelude.Maybe CustomPluginState)
describeCustomPluginResponse_customPluginState = Lens.lens (\DescribeCustomPluginResponse' {customPluginState} -> customPluginState) (\s@DescribeCustomPluginResponse' {} a -> s {customPluginState = a} :: DescribeCustomPluginResponse)

-- | The description of the custom plugin.
describeCustomPluginResponse_description :: Lens.Lens' DescribeCustomPluginResponse (Prelude.Maybe Prelude.Text)
describeCustomPluginResponse_description = Lens.lens (\DescribeCustomPluginResponse' {description} -> description) (\s@DescribeCustomPluginResponse' {} a -> s {description = a} :: DescribeCustomPluginResponse)

-- | The latest successfully created revision of the custom plugin. If there
-- are no successfully created revisions, this field will be absent.
describeCustomPluginResponse_latestRevision :: Lens.Lens' DescribeCustomPluginResponse (Prelude.Maybe CustomPluginRevisionSummary)
describeCustomPluginResponse_latestRevision = Lens.lens (\DescribeCustomPluginResponse' {latestRevision} -> latestRevision) (\s@DescribeCustomPluginResponse' {} a -> s {latestRevision = a} :: DescribeCustomPluginResponse)

-- | The name of the custom plugin.
describeCustomPluginResponse_name :: Lens.Lens' DescribeCustomPluginResponse (Prelude.Maybe Prelude.Text)
describeCustomPluginResponse_name = Lens.lens (\DescribeCustomPluginResponse' {name} -> name) (\s@DescribeCustomPluginResponse' {} a -> s {name = a} :: DescribeCustomPluginResponse)

-- | Details about the state of a custom plugin.
describeCustomPluginResponse_stateDescription :: Lens.Lens' DescribeCustomPluginResponse (Prelude.Maybe StateDescription)
describeCustomPluginResponse_stateDescription = Lens.lens (\DescribeCustomPluginResponse' {stateDescription} -> stateDescription) (\s@DescribeCustomPluginResponse' {} a -> s {stateDescription = a} :: DescribeCustomPluginResponse)

-- | The response's http status code.
describeCustomPluginResponse_httpStatus :: Lens.Lens' DescribeCustomPluginResponse Prelude.Int
describeCustomPluginResponse_httpStatus = Lens.lens (\DescribeCustomPluginResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomPluginResponse' {} a -> s {httpStatus = a} :: DescribeCustomPluginResponse)

instance Prelude.NFData DescribeCustomPluginResponse where
  rnf DescribeCustomPluginResponse' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf customPluginArn `Prelude.seq`
        Prelude.rnf customPluginState `Prelude.seq`
          Prelude.rnf description `Prelude.seq`
            Prelude.rnf latestRevision `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf stateDescription `Prelude.seq`
                  Prelude.rnf httpStatus
