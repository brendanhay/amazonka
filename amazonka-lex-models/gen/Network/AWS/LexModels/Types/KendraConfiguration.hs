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
-- Module      : Network.AWS.LexModels.Types.KendraConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.KendraConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides configuration information for the AMAZON.KendraSearchIntent
-- intent. When you use this intent, Amazon Lex searches the specified
-- Amazon Kendra index and returns documents from the index that match the
-- user\'s utterance. For more information, see
-- <http://docs.aws.amazon.com/lex/latest/dg/built-in-intent-kendra-search.html AMAZON.KendraSearchIntent>.
--
-- /See:/ 'newKendraConfiguration' smart constructor.
data KendraConfiguration = KendraConfiguration'
  { -- | A query filter that Amazon Lex sends to Amazon Kendra to filter the
    -- response from the query. The filter is in the format defined by Amazon
    -- Kendra. For more information, see
    -- <http://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries>.
    --
    -- You can override this filter string with a new filter string at runtime.
    queryFilterString :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Kendra index that you want
    -- the AMAZON.KendraSearchIntent intent to search. The index must be in the
    -- same account and Region as the Amazon Lex bot. If the Amazon Kendra
    -- index does not exist, you get an exception when you call the @PutIntent@
    -- operation.
    kendraIndex :: Core.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role that has permission to
    -- search the Amazon Kendra index. The role must be in the same account and
    -- Region as the Amazon Lex bot. If the role does not exist, you get an
    -- exception when you call the @PutIntent@ operation.
    role' :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KendraConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryFilterString', 'kendraConfiguration_queryFilterString' - A query filter that Amazon Lex sends to Amazon Kendra to filter the
-- response from the query. The filter is in the format defined by Amazon
-- Kendra. For more information, see
-- <http://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries>.
--
-- You can override this filter string with a new filter string at runtime.
--
-- 'kendraIndex', 'kendraConfiguration_kendraIndex' - The Amazon Resource Name (ARN) of the Amazon Kendra index that you want
-- the AMAZON.KendraSearchIntent intent to search. The index must be in the
-- same account and Region as the Amazon Lex bot. If the Amazon Kendra
-- index does not exist, you get an exception when you call the @PutIntent@
-- operation.
--
-- 'role'', 'kendraConfiguration_role' - The Amazon Resource Name (ARN) of an IAM role that has permission to
-- search the Amazon Kendra index. The role must be in the same account and
-- Region as the Amazon Lex bot. If the role does not exist, you get an
-- exception when you call the @PutIntent@ operation.
newKendraConfiguration ::
  -- | 'kendraIndex'
  Core.Text ->
  -- | 'role''
  Core.Text ->
  KendraConfiguration
newKendraConfiguration pKendraIndex_ pRole_ =
  KendraConfiguration'
    { queryFilterString =
        Core.Nothing,
      kendraIndex = pKendraIndex_,
      role' = pRole_
    }

-- | A query filter that Amazon Lex sends to Amazon Kendra to filter the
-- response from the query. The filter is in the format defined by Amazon
-- Kendra. For more information, see
-- <http://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries>.
--
-- You can override this filter string with a new filter string at runtime.
kendraConfiguration_queryFilterString :: Lens.Lens' KendraConfiguration (Core.Maybe Core.Text)
kendraConfiguration_queryFilterString = Lens.lens (\KendraConfiguration' {queryFilterString} -> queryFilterString) (\s@KendraConfiguration' {} a -> s {queryFilterString = a} :: KendraConfiguration)

-- | The Amazon Resource Name (ARN) of the Amazon Kendra index that you want
-- the AMAZON.KendraSearchIntent intent to search. The index must be in the
-- same account and Region as the Amazon Lex bot. If the Amazon Kendra
-- index does not exist, you get an exception when you call the @PutIntent@
-- operation.
kendraConfiguration_kendraIndex :: Lens.Lens' KendraConfiguration Core.Text
kendraConfiguration_kendraIndex = Lens.lens (\KendraConfiguration' {kendraIndex} -> kendraIndex) (\s@KendraConfiguration' {} a -> s {kendraIndex = a} :: KendraConfiguration)

-- | The Amazon Resource Name (ARN) of an IAM role that has permission to
-- search the Amazon Kendra index. The role must be in the same account and
-- Region as the Amazon Lex bot. If the role does not exist, you get an
-- exception when you call the @PutIntent@ operation.
kendraConfiguration_role :: Lens.Lens' KendraConfiguration Core.Text
kendraConfiguration_role = Lens.lens (\KendraConfiguration' {role'} -> role') (\s@KendraConfiguration' {} a -> s {role' = a} :: KendraConfiguration)

instance Core.FromJSON KendraConfiguration where
  parseJSON =
    Core.withObject
      "KendraConfiguration"
      ( \x ->
          KendraConfiguration'
            Core.<$> (x Core..:? "queryFilterString")
            Core.<*> (x Core..: "kendraIndex")
            Core.<*> (x Core..: "role")
      )

instance Core.Hashable KendraConfiguration

instance Core.NFData KendraConfiguration

instance Core.ToJSON KendraConfiguration where
  toJSON KendraConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("queryFilterString" Core..=)
              Core.<$> queryFilterString,
            Core.Just ("kendraIndex" Core..= kendraIndex),
            Core.Just ("role" Core..= role')
          ]
      )
