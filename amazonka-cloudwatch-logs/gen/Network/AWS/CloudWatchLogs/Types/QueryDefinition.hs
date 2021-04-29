{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.QueryDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.QueryDefinition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This structure contains details about a saved CloudWatch Logs Insights
-- query definition.
--
-- /See:/ 'newQueryDefinition' smart constructor.
data QueryDefinition = QueryDefinition'
  { -- | The query string to use for this definition. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax>.
    queryString :: Prelude.Maybe Prelude.Text,
    -- | The name of the query definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | If this query definition contains a list of log groups that it is
    -- limited to, that list appears here.
    logGroupNames :: Prelude.Maybe [Prelude.Text],
    -- | The date that the query definition was most recently modified.
    lastModified :: Prelude.Maybe Prelude.Natural,
    -- | The unique ID of the query definition.
    queryDefinitionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'QueryDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryString', 'queryDefinition_queryString' - The query string to use for this definition. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax>.
--
-- 'name', 'queryDefinition_name' - The name of the query definition.
--
-- 'logGroupNames', 'queryDefinition_logGroupNames' - If this query definition contains a list of log groups that it is
-- limited to, that list appears here.
--
-- 'lastModified', 'queryDefinition_lastModified' - The date that the query definition was most recently modified.
--
-- 'queryDefinitionId', 'queryDefinition_queryDefinitionId' - The unique ID of the query definition.
newQueryDefinition ::
  QueryDefinition
newQueryDefinition =
  QueryDefinition'
    { queryString = Prelude.Nothing,
      name = Prelude.Nothing,
      logGroupNames = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      queryDefinitionId = Prelude.Nothing
    }

-- | The query string to use for this definition. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax>.
queryDefinition_queryString :: Lens.Lens' QueryDefinition (Prelude.Maybe Prelude.Text)
queryDefinition_queryString = Lens.lens (\QueryDefinition' {queryString} -> queryString) (\s@QueryDefinition' {} a -> s {queryString = a} :: QueryDefinition)

-- | The name of the query definition.
queryDefinition_name :: Lens.Lens' QueryDefinition (Prelude.Maybe Prelude.Text)
queryDefinition_name = Lens.lens (\QueryDefinition' {name} -> name) (\s@QueryDefinition' {} a -> s {name = a} :: QueryDefinition)

-- | If this query definition contains a list of log groups that it is
-- limited to, that list appears here.
queryDefinition_logGroupNames :: Lens.Lens' QueryDefinition (Prelude.Maybe [Prelude.Text])
queryDefinition_logGroupNames = Lens.lens (\QueryDefinition' {logGroupNames} -> logGroupNames) (\s@QueryDefinition' {} a -> s {logGroupNames = a} :: QueryDefinition) Prelude.. Lens.mapping Prelude._Coerce

-- | The date that the query definition was most recently modified.
queryDefinition_lastModified :: Lens.Lens' QueryDefinition (Prelude.Maybe Prelude.Natural)
queryDefinition_lastModified = Lens.lens (\QueryDefinition' {lastModified} -> lastModified) (\s@QueryDefinition' {} a -> s {lastModified = a} :: QueryDefinition)

-- | The unique ID of the query definition.
queryDefinition_queryDefinitionId :: Lens.Lens' QueryDefinition (Prelude.Maybe Prelude.Text)
queryDefinition_queryDefinitionId = Lens.lens (\QueryDefinition' {queryDefinitionId} -> queryDefinitionId) (\s@QueryDefinition' {} a -> s {queryDefinitionId = a} :: QueryDefinition)

instance Prelude.FromJSON QueryDefinition where
  parseJSON =
    Prelude.withObject
      "QueryDefinition"
      ( \x ->
          QueryDefinition'
            Prelude.<$> (x Prelude..:? "queryString")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> ( x Prelude..:? "logGroupNames"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "lastModified")
            Prelude.<*> (x Prelude..:? "queryDefinitionId")
      )

instance Prelude.Hashable QueryDefinition

instance Prelude.NFData QueryDefinition
