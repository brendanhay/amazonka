{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.KendraConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.KendraConfiguration
  ( KendraConfiguration (..),

    -- * Smart constructor
    mkKendraConfiguration,

    -- * Lenses
    kcKendraIndex,
    kcRole,
    kcQueryFilterString,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.KendraIndexArn as Types
import qualified Network.AWS.LexModels.Types.QueryFilterString as Types
import qualified Network.AWS.LexModels.Types.Role as Types
import qualified Network.AWS.Prelude as Core

-- | Provides configuration information for the AMAZON.KendraSearchIntent intent. When you use this intent, Amazon Lex searches the specified Amazon Kendra index and returns documents from the index that match the user's utterance. For more information, see <http://docs.aws.amazon.com/lex/latest/dg/built-in-intent-kendra-search.html AMAZON.KendraSearchIntent> .
--
-- /See:/ 'mkKendraConfiguration' smart constructor.
data KendraConfiguration = KendraConfiguration'
  { -- | The Amazon Resource Name (ARN) of the Amazon Kendra index that you want the AMAZON.KendraSearchIntent intent to search. The index must be in the same account and Region as the Amazon Lex bot. If the Amazon Kendra index does not exist, you get an exception when you call the @PutIntent@ operation.
    kendraIndex :: Types.KendraIndexArn,
    -- | The Amazon Resource Name (ARN) of an IAM role that has permission to search the Amazon Kendra index. The role must be in the same account and Region as the Amazon Lex bot. If the role does not exist, you get an exception when you call the @PutIntent@ operation.
    role' :: Types.Role,
    -- | A query filter that Amazon Lex sends to Amazon Kendra to filter the response from the query. The filter is in the format defined by Amazon Kendra. For more information, see <http://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries> .
    --
    -- You can override this filter string with a new filter string at runtime.
    queryFilterString :: Core.Maybe Types.QueryFilterString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KendraConfiguration' value with any optional fields omitted.
mkKendraConfiguration ::
  -- | 'kendraIndex'
  Types.KendraIndexArn ->
  -- | 'role\''
  Types.Role ->
  KendraConfiguration
mkKendraConfiguration kendraIndex role' =
  KendraConfiguration'
    { kendraIndex,
      role',
      queryFilterString = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon Kendra index that you want the AMAZON.KendraSearchIntent intent to search. The index must be in the same account and Region as the Amazon Lex bot. If the Amazon Kendra index does not exist, you get an exception when you call the @PutIntent@ operation.
--
-- /Note:/ Consider using 'kendraIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kcKendraIndex :: Lens.Lens' KendraConfiguration Types.KendraIndexArn
kcKendraIndex = Lens.field @"kendraIndex"
{-# DEPRECATED kcKendraIndex "Use generic-lens or generic-optics with 'kendraIndex' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that has permission to search the Amazon Kendra index. The role must be in the same account and Region as the Amazon Lex bot. If the role does not exist, you get an exception when you call the @PutIntent@ operation.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kcRole :: Lens.Lens' KendraConfiguration Types.Role
kcRole = Lens.field @"role'"
{-# DEPRECATED kcRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | A query filter that Amazon Lex sends to Amazon Kendra to filter the response from the query. The filter is in the format defined by Amazon Kendra. For more information, see <http://docs.aws.amazon.com/kendra/latest/dg/filtering.html Filtering queries> .
--
-- You can override this filter string with a new filter string at runtime.
--
-- /Note:/ Consider using 'queryFilterString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kcQueryFilterString :: Lens.Lens' KendraConfiguration (Core.Maybe Types.QueryFilterString)
kcQueryFilterString = Lens.field @"queryFilterString"
{-# DEPRECATED kcQueryFilterString "Use generic-lens or generic-optics with 'queryFilterString' instead." #-}

instance Core.FromJSON KendraConfiguration where
  toJSON KendraConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("kendraIndex" Core..= kendraIndex),
            Core.Just ("role" Core..= role'),
            ("queryFilterString" Core..=) Core.<$> queryFilterString
          ]
      )

instance Core.FromJSON KendraConfiguration where
  parseJSON =
    Core.withObject "KendraConfiguration" Core.$
      \x ->
        KendraConfiguration'
          Core.<$> (x Core..: "kendraIndex")
          Core.<*> (x Core..: "role")
          Core.<*> (x Core..:? "queryFilterString")
