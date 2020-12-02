{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ConstraintDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ConstraintDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a constraint.
--
--
--
-- /See:/ 'constraintDetail' smart constructor.
data ConstraintDetail = ConstraintDetail'
  { _cdPortfolioId ::
      !(Maybe Text),
    _cdConstraintId :: !(Maybe Text),
    _cdOwner :: !(Maybe Text),
    _cdType :: !(Maybe Text),
    _cdDescription :: !(Maybe Text),
    _cdProductId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConstraintDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdPortfolioId' - The identifier of the portfolio the product resides in. The constraint applies only to the instance of the product that lives within this portfolio.
--
-- * 'cdConstraintId' - The identifier of the constraint.
--
-- * 'cdOwner' - The owner of the constraint.
--
-- * 'cdType' - The type of constraint.     * @LAUNCH@      * @NOTIFICATION@      * STACKSET     * @TEMPLATE@
--
-- * 'cdDescription' - The description of the constraint.
--
-- * 'cdProductId' - The identifier of the product the constraint applies to. Note that a constraint applies to a specific instance of a product within a certain portfolio.
constraintDetail ::
  ConstraintDetail
constraintDetail =
  ConstraintDetail'
    { _cdPortfolioId = Nothing,
      _cdConstraintId = Nothing,
      _cdOwner = Nothing,
      _cdType = Nothing,
      _cdDescription = Nothing,
      _cdProductId = Nothing
    }

-- | The identifier of the portfolio the product resides in. The constraint applies only to the instance of the product that lives within this portfolio.
cdPortfolioId :: Lens' ConstraintDetail (Maybe Text)
cdPortfolioId = lens _cdPortfolioId (\s a -> s {_cdPortfolioId = a})

-- | The identifier of the constraint.
cdConstraintId :: Lens' ConstraintDetail (Maybe Text)
cdConstraintId = lens _cdConstraintId (\s a -> s {_cdConstraintId = a})

-- | The owner of the constraint.
cdOwner :: Lens' ConstraintDetail (Maybe Text)
cdOwner = lens _cdOwner (\s a -> s {_cdOwner = a})

-- | The type of constraint.     * @LAUNCH@      * @NOTIFICATION@      * STACKSET     * @TEMPLATE@
cdType :: Lens' ConstraintDetail (Maybe Text)
cdType = lens _cdType (\s a -> s {_cdType = a})

-- | The description of the constraint.
cdDescription :: Lens' ConstraintDetail (Maybe Text)
cdDescription = lens _cdDescription (\s a -> s {_cdDescription = a})

-- | The identifier of the product the constraint applies to. Note that a constraint applies to a specific instance of a product within a certain portfolio.
cdProductId :: Lens' ConstraintDetail (Maybe Text)
cdProductId = lens _cdProductId (\s a -> s {_cdProductId = a})

instance FromJSON ConstraintDetail where
  parseJSON =
    withObject
      "ConstraintDetail"
      ( \x ->
          ConstraintDetail'
            <$> (x .:? "PortfolioId")
            <*> (x .:? "ConstraintId")
            <*> (x .:? "Owner")
            <*> (x .:? "Type")
            <*> (x .:? "Description")
            <*> (x .:? "ProductId")
      )

instance Hashable ConstraintDetail

instance NFData ConstraintDetail
