{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeRulesPackages
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the rules packages that are specified by the ARNs of the rules packages.
--
--
module Network.AWS.Inspector.DescribeRulesPackages
    (
    -- * Creating a Request
      describeRulesPackages
    , DescribeRulesPackages
    -- * Request Lenses
    , drpLocale
    , drpRulesPackageARNs

    -- * Destructuring the Response
    , describeRulesPackagesResponse
    , DescribeRulesPackagesResponse
    -- * Response Lenses
    , drprsResponseStatus
    , drprsRulesPackages
    , drprsFailedItems
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRulesPackages' smart constructor.
data DescribeRulesPackages = DescribeRulesPackages'
  { _drpLocale           :: !(Maybe Locale)
  , _drpRulesPackageARNs :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRulesPackages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpLocale' - The locale that you want to translate a rules package description into.
--
-- * 'drpRulesPackageARNs' - The ARN that specifies the rules package that you want to describe.
describeRulesPackages
    :: NonEmpty Text -- ^ 'drpRulesPackageARNs'
    -> DescribeRulesPackages
describeRulesPackages pRulesPackageARNs_ =
  DescribeRulesPackages'
    {_drpLocale = Nothing, _drpRulesPackageARNs = _List1 # pRulesPackageARNs_}


-- | The locale that you want to translate a rules package description into.
drpLocale :: Lens' DescribeRulesPackages (Maybe Locale)
drpLocale = lens _drpLocale (\ s a -> s{_drpLocale = a})

-- | The ARN that specifies the rules package that you want to describe.
drpRulesPackageARNs :: Lens' DescribeRulesPackages (NonEmpty Text)
drpRulesPackageARNs = lens _drpRulesPackageARNs (\ s a -> s{_drpRulesPackageARNs = a}) . _List1

instance AWSRequest DescribeRulesPackages where
        type Rs DescribeRulesPackages =
             DescribeRulesPackagesResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRulesPackagesResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "rulesPackages" .!@ mempty)
                     <*> (x .?> "failedItems" .!@ mempty))

instance Hashable DescribeRulesPackages where

instance NFData DescribeRulesPackages where

instance ToHeaders DescribeRulesPackages where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DescribeRulesPackages" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRulesPackages where
        toJSON DescribeRulesPackages'{..}
          = object
              (catMaybes
                 [("locale" .=) <$> _drpLocale,
                  Just ("rulesPackageArns" .= _drpRulesPackageARNs)])

instance ToPath DescribeRulesPackages where
        toPath = const "/"

instance ToQuery DescribeRulesPackages where
        toQuery = const mempty

-- | /See:/ 'describeRulesPackagesResponse' smart constructor.
data DescribeRulesPackagesResponse = DescribeRulesPackagesResponse'
  { _drprsResponseStatus :: !Int
  , _drprsRulesPackages  :: ![RulesPackage]
  , _drprsFailedItems    :: !(Map Text FailedItemDetails)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRulesPackagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drprsResponseStatus' - -- | The response status code.
--
-- * 'drprsRulesPackages' - Information about the rules package.
--
-- * 'drprsFailedItems' - Rules package details that cannot be described. An error code is provided for each failed item.
describeRulesPackagesResponse
    :: Int -- ^ 'drprsResponseStatus'
    -> DescribeRulesPackagesResponse
describeRulesPackagesResponse pResponseStatus_ =
  DescribeRulesPackagesResponse'
    { _drprsResponseStatus = pResponseStatus_
    , _drprsRulesPackages = mempty
    , _drprsFailedItems = mempty
    }


-- | -- | The response status code.
drprsResponseStatus :: Lens' DescribeRulesPackagesResponse Int
drprsResponseStatus = lens _drprsResponseStatus (\ s a -> s{_drprsResponseStatus = a})

-- | Information about the rules package.
drprsRulesPackages :: Lens' DescribeRulesPackagesResponse [RulesPackage]
drprsRulesPackages = lens _drprsRulesPackages (\ s a -> s{_drprsRulesPackages = a}) . _Coerce

-- | Rules package details that cannot be described. An error code is provided for each failed item.
drprsFailedItems :: Lens' DescribeRulesPackagesResponse (HashMap Text FailedItemDetails)
drprsFailedItems = lens _drprsFailedItems (\ s a -> s{_drprsFailedItems = a}) . _Map

instance NFData DescribeRulesPackagesResponse where
