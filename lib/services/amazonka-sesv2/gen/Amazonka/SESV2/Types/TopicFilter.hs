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
-- Module      : Amazonka.SESV2.Types.TopicFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.TopicFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Used for filtering by a specific topic preference.
--
-- /See:/ 'newTopicFilter' smart constructor.
data TopicFilter = TopicFilter'
  { -- | The name of a topic on which you wish to apply the filter.
    topicName :: Prelude.Maybe Prelude.Text,
    -- | Notes that the default subscription status should be applied to a
    -- contact because the contact has not noted their preference for
    -- subscribing to a topic.
    useDefaultIfPreferenceUnavailable :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicName', 'topicFilter_topicName' - The name of a topic on which you wish to apply the filter.
--
-- 'useDefaultIfPreferenceUnavailable', 'topicFilter_useDefaultIfPreferenceUnavailable' - Notes that the default subscription status should be applied to a
-- contact because the contact has not noted their preference for
-- subscribing to a topic.
newTopicFilter ::
  TopicFilter
newTopicFilter =
  TopicFilter'
    { topicName = Prelude.Nothing,
      useDefaultIfPreferenceUnavailable = Prelude.Nothing
    }

-- | The name of a topic on which you wish to apply the filter.
topicFilter_topicName :: Lens.Lens' TopicFilter (Prelude.Maybe Prelude.Text)
topicFilter_topicName = Lens.lens (\TopicFilter' {topicName} -> topicName) (\s@TopicFilter' {} a -> s {topicName = a} :: TopicFilter)

-- | Notes that the default subscription status should be applied to a
-- contact because the contact has not noted their preference for
-- subscribing to a topic.
topicFilter_useDefaultIfPreferenceUnavailable :: Lens.Lens' TopicFilter (Prelude.Maybe Prelude.Bool)
topicFilter_useDefaultIfPreferenceUnavailable = Lens.lens (\TopicFilter' {useDefaultIfPreferenceUnavailable} -> useDefaultIfPreferenceUnavailable) (\s@TopicFilter' {} a -> s {useDefaultIfPreferenceUnavailable = a} :: TopicFilter)

instance Prelude.Hashable TopicFilter where
  hashWithSalt _salt TopicFilter' {..} =
    _salt
      `Prelude.hashWithSalt` topicName
      `Prelude.hashWithSalt` useDefaultIfPreferenceUnavailable

instance Prelude.NFData TopicFilter where
  rnf TopicFilter' {..} =
    Prelude.rnf topicName `Prelude.seq`
      Prelude.rnf useDefaultIfPreferenceUnavailable

instance Data.ToJSON TopicFilter where
  toJSON TopicFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TopicName" Data..=) Prelude.<$> topicName,
            ("UseDefaultIfPreferenceUnavailable" Data..=)
              Prelude.<$> useDefaultIfPreferenceUnavailable
          ]
      )
