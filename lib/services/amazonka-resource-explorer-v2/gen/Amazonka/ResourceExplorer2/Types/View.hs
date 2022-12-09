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
-- Module      : Amazonka.ResourceExplorer2.Types.View
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceExplorer2.Types.View where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResourceExplorer2.Types.IncludedProperty
import Amazonka.ResourceExplorer2.Types.SearchFilter

-- | A view is a structure that defines a set of filters that provide a view
-- into the information in the Amazon Web Services Resource Explorer index.
-- The filters specify which information from the index is visible to the
-- users of the view. For example, you can specify filters that include
-- only resources that are tagged with the key \"ENV\" and the value
-- \"DEVELOPMENT\" in the results returned by this view. You could also
-- create a second view that includes only resources that are tagged with
-- \"ENV\" and \"PRODUCTION\".
--
-- /See:/ 'newView' smart constructor.
data View = View'
  { -- | An array of SearchFilter objects that specify which resources can be
    -- included in the results of queries made using this view.
    filters :: Prelude.Maybe (Data.Sensitive SearchFilter),
    -- | A structure that contains additional information about the view.
    includedProperties :: Prelude.Maybe [IncludedProperty],
    -- | The date and time when this view was last modified.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Web Services account that owns this view.
    owner :: Prelude.Maybe Prelude.Text,
    -- | An
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of an Amazon Web Services account, an organization, or an organizational
    -- unit (OU) that specifies whether this view includes resources from only
    -- the specified Amazon Web Services account, all accounts in the specified
    -- organization, or all accounts in the specified OU.
    --
    -- If not specified, the value defaults to the Amazon Web Services account
    -- used to call this operation.
    scope :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of the view.
    viewArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'View' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'view_filters' - An array of SearchFilter objects that specify which resources can be
-- included in the results of queries made using this view.
--
-- 'includedProperties', 'view_includedProperties' - A structure that contains additional information about the view.
--
-- 'lastUpdatedAt', 'view_lastUpdatedAt' - The date and time when this view was last modified.
--
-- 'owner', 'view_owner' - The Amazon Web Services account that owns this view.
--
-- 'scope', 'view_scope' - An
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of an Amazon Web Services account, an organization, or an organizational
-- unit (OU) that specifies whether this view includes resources from only
-- the specified Amazon Web Services account, all accounts in the specified
-- organization, or all accounts in the specified OU.
--
-- If not specified, the value defaults to the Amazon Web Services account
-- used to call this operation.
--
-- 'viewArn', 'view_viewArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view.
newView ::
  View
newView =
  View'
    { filters = Prelude.Nothing,
      includedProperties = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      owner = Prelude.Nothing,
      scope = Prelude.Nothing,
      viewArn = Prelude.Nothing
    }

-- | An array of SearchFilter objects that specify which resources can be
-- included in the results of queries made using this view.
view_filters :: Lens.Lens' View (Prelude.Maybe SearchFilter)
view_filters = Lens.lens (\View' {filters} -> filters) (\s@View' {} a -> s {filters = a} :: View) Prelude.. Lens.mapping Data._Sensitive

-- | A structure that contains additional information about the view.
view_includedProperties :: Lens.Lens' View (Prelude.Maybe [IncludedProperty])
view_includedProperties = Lens.lens (\View' {includedProperties} -> includedProperties) (\s@View' {} a -> s {includedProperties = a} :: View) Prelude.. Lens.mapping Lens.coerced

-- | The date and time when this view was last modified.
view_lastUpdatedAt :: Lens.Lens' View (Prelude.Maybe Prelude.UTCTime)
view_lastUpdatedAt = Lens.lens (\View' {lastUpdatedAt} -> lastUpdatedAt) (\s@View' {} a -> s {lastUpdatedAt = a} :: View) Prelude.. Lens.mapping Data._Time

-- | The Amazon Web Services account that owns this view.
view_owner :: Lens.Lens' View (Prelude.Maybe Prelude.Text)
view_owner = Lens.lens (\View' {owner} -> owner) (\s@View' {} a -> s {owner = a} :: View)

-- | An
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of an Amazon Web Services account, an organization, or an organizational
-- unit (OU) that specifies whether this view includes resources from only
-- the specified Amazon Web Services account, all accounts in the specified
-- organization, or all accounts in the specified OU.
--
-- If not specified, the value defaults to the Amazon Web Services account
-- used to call this operation.
view_scope :: Lens.Lens' View (Prelude.Maybe Prelude.Text)
view_scope = Lens.lens (\View' {scope} -> scope) (\s@View' {} a -> s {scope = a} :: View)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view.
view_viewArn :: Lens.Lens' View (Prelude.Maybe Prelude.Text)
view_viewArn = Lens.lens (\View' {viewArn} -> viewArn) (\s@View' {} a -> s {viewArn = a} :: View)

instance Data.FromJSON View where
  parseJSON =
    Data.withObject
      "View"
      ( \x ->
          View'
            Prelude.<$> (x Data..:? "Filters")
            Prelude.<*> ( x Data..:? "IncludedProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "LastUpdatedAt")
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "Scope")
            Prelude.<*> (x Data..:? "ViewArn")
      )

instance Prelude.Hashable View where
  hashWithSalt _salt View' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` includedProperties
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` viewArn

instance Prelude.NFData View where
  rnf View' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf includedProperties
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf viewArn
