{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This module is provided for convenience. It offers an alternative to the
-- common idiom of supplying required fields to an operations's smart constructor,
-- using the operation's lenses to modify additional fields, and then sending
-- or paginating the request.
--
-- As an example: using "Network.AWS.CloudSearch" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.CloudSearch
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.CloudSearch.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Control.Applicative
-- import Network.AWS.CloudSearch.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
--
-- -- Or to void any additional parameters outside of those required using 'Control.Applicative.empty':
-- operationName w x empty
-- @
--
module Network.AWS.CloudSearch.Monadic
    (
    -- * BuildSuggesters
    -- $BuildSuggesters
      buildSuggesters
    , buildSuggestersCatch

    -- * CreateDomain
    -- $CreateDomain
    , createDomain
    , createDomainCatch

    -- * DefineAnalysisScheme
    -- $DefineAnalysisScheme
    , defineAnalysisScheme
    , defineAnalysisSchemeCatch

    -- * DefineExpression
    -- $DefineExpression
    , defineExpression
    , defineExpressionCatch

    -- * DefineIndexField
    -- $DefineIndexField
    , defineIndexField
    , defineIndexFieldCatch

    -- * DefineSuggester
    -- $DefineSuggester
    , defineSuggester
    , defineSuggesterCatch

    -- * DeleteAnalysisScheme
    -- $DeleteAnalysisScheme
    , deleteAnalysisScheme
    , deleteAnalysisSchemeCatch

    -- * DeleteDomain
    -- $DeleteDomain
    , deleteDomain
    , deleteDomainCatch

    -- * DeleteExpression
    -- $DeleteExpression
    , deleteExpression
    , deleteExpressionCatch

    -- * DeleteIndexField
    -- $DeleteIndexField
    , deleteIndexField
    , deleteIndexFieldCatch

    -- * DeleteSuggester
    -- $DeleteSuggester
    , deleteSuggester
    , deleteSuggesterCatch

    -- * DescribeAnalysisSchemes
    -- $DescribeAnalysisSchemes
    , describeAnalysisSchemes
    , describeAnalysisSchemesCatch

    -- * DescribeAvailabilityOptions
    -- $DescribeAvailabilityOptions
    , describeAvailabilityOptions
    , describeAvailabilityOptionsCatch

    -- * DescribeDomains
    -- $DescribeDomains
    , describeDomains
    , describeDomainsCatch

    -- * DescribeExpressions
    -- $DescribeExpressions
    , describeExpressions
    , describeExpressionsCatch

    -- * DescribeIndexFields
    -- $DescribeIndexFields
    , describeIndexFields
    , describeIndexFieldsCatch

    -- * DescribeScalingParameters
    -- $DescribeScalingParameters
    , describeScalingParameters
    , describeScalingParametersCatch

    -- * DescribeServiceAccessPolicies
    -- $DescribeServiceAccessPolicies
    , describeServiceAccessPolicies
    , describeServiceAccessPoliciesCatch

    -- * DescribeSuggesters
    -- $DescribeSuggesters
    , describeSuggesters
    , describeSuggestersCatch

    -- * IndexDocuments
    -- $IndexDocuments
    , indexDocuments
    , indexDocumentsCatch

    -- * ListDomainNames
    -- $ListDomainNames
    , listDomainNames
    , listDomainNamesCatch

    -- * UpdateAvailabilityOptions
    -- $UpdateAvailabilityOptions
    , updateAvailabilityOptions
    , updateAvailabilityOptionsCatch

    -- * UpdateScalingParameters
    -- $UpdateScalingParameters
    , updateScalingParameters
    , updateScalingParametersCatch

    -- * UpdateServiceAccessPolicies
    -- $UpdateServiceAccessPolicies
    , updateServiceAccessPolicies
    , updateServiceAccessPoliciesCatch

    -- * Re-exported
    , module Network.AWS.CloudSearch

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.CloudSearch

type ServiceEr = Er CloudSearch

-- $BuildSuggesters
-- Indexes the search suggestions.
--
-- See: 'Network.AWS.CloudSearch.BuildSuggesters'

buildSuggesters :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'bsDomainName'
    -> m BuildSuggestersResponse
buildSuggesters p1 =
    send (mkBuildSuggesters p1)

buildSuggestersCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'bsDomainName'
    -> m (Either ServiceEr BuildSuggestersResponse)
buildSuggestersCatch p1 =
    sendCatch (mkBuildSuggesters p1)

-- $CreateDomain
-- Creates a new search domain. For more information, see Creating a Search
-- Domain in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.CreateDomain'

createDomain :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'cdDomainName'
    -> m CreateDomainResponse
createDomain p1 =
    send (mkCreateDomain p1)

createDomainCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'cdDomainName'
    -> m (Either ServiceEr CreateDomainResponse)
createDomainCatch p1 =
    sendCatch (mkCreateDomain p1)

-- $DefineAnalysisScheme
-- Configures an analysis scheme that can be applied to a text or text-array
-- field to define language-specific text processing options. For more
-- information, see Configuring Analysis Schemes in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.DefineAnalysisScheme'

defineAnalysisScheme :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'dasDomainName'
    -> AnalysisScheme -- ^ 'dasAnalysisScheme'
    -> m DefineAnalysisSchemeResponse
defineAnalysisScheme p1 p2 =
    send (mkDefineAnalysisScheme p1 p2)

defineAnalysisSchemeCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'dasDomainName'
    -> AnalysisScheme -- ^ 'dasAnalysisScheme'
    -> m (Either ServiceEr DefineAnalysisSchemeResponse)
defineAnalysisSchemeCatch p1 p2 =
    sendCatch (mkDefineAnalysisScheme p1 p2)

-- $DefineExpression
-- Configures an Expression for the search domain. Used to create new
-- expressions and modify existing ones. If the expression exists, the new
-- configuration replaces the old one. For more information, see Configuring
-- Expressions in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.DefineExpression'

defineExpression :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'deDomainName'
    -> Expression -- ^ 'deExpression'
    -> m DefineExpressionResponse
defineExpression p1 p2 =
    send (mkDefineExpression p1 p2)

defineExpressionCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'deDomainName'
    -> Expression -- ^ 'deExpression'
    -> m (Either ServiceEr DefineExpressionResponse)
defineExpressionCatch p1 p2 =
    sendCatch (mkDefineExpression p1 p2)

-- $DefineIndexField
-- Configures an IndexField for the search domain. Used to create new fields
-- and modify existing ones. You must specify the name of the domain you are
-- configuring and an index field configuration. The index field configuration
-- specifies a unique name, the index field type, and the options you want to
-- configure for the field. The options you can specify depend on the
-- IndexFieldType. If the field exists, the new configuration replaces the old
-- one. For more information, see Configuring Index Fields in the Amazon
-- CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.DefineIndexField'

defineIndexField :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'difDomainName'
    -> IndexField -- ^ 'difIndexField'
    -> m DefineIndexFieldResponse
defineIndexField p1 p2 =
    send (mkDefineIndexField p1 p2)

defineIndexFieldCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'difDomainName'
    -> IndexField -- ^ 'difIndexField'
    -> m (Either ServiceEr DefineIndexFieldResponse)
defineIndexFieldCatch p1 p2 =
    sendCatch (mkDefineIndexField p1 p2)

-- $DefineSuggester
-- Configures a suggester for a domain. A suggester enables you to display
-- possible matches before users finish typing their queries. When you
-- configure a suggester, you must specify the name of the text field you want
-- to search for possible matches and a unique name for the suggester. For
-- more information, see Getting Search Suggestions in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.DefineSuggester'

defineSuggester :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'ds1DomainName'
    -> Suggester -- ^ 'ds1Suggester'
    -> m DefineSuggesterResponse
defineSuggester p1 p2 =
    send (mkDefineSuggester p1 p2)

defineSuggesterCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'ds1DomainName'
    -> Suggester -- ^ 'ds1Suggester'
    -> m (Either ServiceEr DefineSuggesterResponse)
defineSuggesterCatch p1 p2 =
    sendCatch (mkDefineSuggester p1 p2)

-- $DeleteAnalysisScheme
-- Deletes an analysis scheme. For more information, see Configuring Analysis
-- Schemes in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.DeleteAnalysisScheme'

deleteAnalysisScheme :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'das1DomainName'
    -> Text -- ^ 'das1AnalysisSchemeName'
    -> m DeleteAnalysisSchemeResponse
deleteAnalysisScheme p1 p2 =
    send (mkDeleteAnalysisScheme p1 p2)

deleteAnalysisSchemeCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'das1DomainName'
    -> Text -- ^ 'das1AnalysisSchemeName'
    -> m (Either ServiceEr DeleteAnalysisSchemeResponse)
deleteAnalysisSchemeCatch p1 p2 =
    sendCatch (mkDeleteAnalysisScheme p1 p2)

-- $DeleteDomain
-- Permanently deletes a search domain and all of its data. Once a domain has
-- been deleted, it cannot be recovered. For more information, see Deleting a
-- Search Domain in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.DeleteDomain'

deleteDomain :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'ddDomainName'
    -> m DeleteDomainResponse
deleteDomain p1 =
    send (mkDeleteDomain p1)

deleteDomainCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'ddDomainName'
    -> m (Either ServiceEr DeleteDomainResponse)
deleteDomainCatch p1 =
    sendCatch (mkDeleteDomain p1)

-- $DeleteExpression
-- Removes an Expression from the search domain. For more information, see
-- Configuring Expressions in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.DeleteExpression'

deleteExpression :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'de1DomainName'
    -> Text -- ^ 'de1ExpressionName'
    -> m DeleteExpressionResponse
deleteExpression p1 p2 =
    send (mkDeleteExpression p1 p2)

deleteExpressionCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'de1DomainName'
    -> Text -- ^ 'de1ExpressionName'
    -> m (Either ServiceEr DeleteExpressionResponse)
deleteExpressionCatch p1 p2 =
    sendCatch (mkDeleteExpression p1 p2)

-- $DeleteIndexField
-- Removes an IndexField from the search domain. For more information, see
-- Configuring Index Fields in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.DeleteIndexField'

deleteIndexField :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'dif1DomainName'
    -> Text -- ^ 'dif1IndexFieldName'
    -> m DeleteIndexFieldResponse
deleteIndexField p1 p2 =
    send (mkDeleteIndexField p1 p2)

deleteIndexFieldCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'dif1DomainName'
    -> Text -- ^ 'dif1IndexFieldName'
    -> m (Either ServiceEr DeleteIndexFieldResponse)
deleteIndexFieldCatch p1 p2 =
    sendCatch (mkDeleteIndexField p1 p2)

-- $DeleteSuggester
-- Deletes a suggester. For more information, see Getting Search Suggestions
-- in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.DeleteSuggester'

deleteSuggester :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'ds2DomainName'
    -> Text -- ^ 'ds2SuggesterName'
    -> m DeleteSuggesterResponse
deleteSuggester p1 p2 =
    send (mkDeleteSuggester p1 p2)

deleteSuggesterCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'ds2DomainName'
    -> Text -- ^ 'ds2SuggesterName'
    -> m (Either ServiceEr DeleteSuggesterResponse)
deleteSuggesterCatch p1 p2 =
    sendCatch (mkDeleteSuggester p1 p2)

-- $DescribeAnalysisSchemes
-- Gets the analysis schemes configured for a domain. An analysis scheme
-- defines language-specific text processing options for a text field. Can be
-- limited to specific analysis schemes by name. By default, shows all
-- analysis schemes and includes any pending changes to the configuration. Set
-- the Deployed option to true to show the active configuration and exclude
-- pending changes. For more information, see Configuring Analysis Schemes in
-- the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.DescribeAnalysisSchemes'

describeAnalysisSchemes :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => Text -- ^ 'das2DomainName'
    -> State DescribeAnalysisSchemes a
    -> m DescribeAnalysisSchemesResponse
describeAnalysisSchemes p1 s =
    send $ (mkDescribeAnalysisSchemes p1) &~ s

describeAnalysisSchemesCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => Text -- ^ 'das2DomainName'
    -> State DescribeAnalysisSchemes a
    -> m (Either ServiceEr DescribeAnalysisSchemesResponse)
describeAnalysisSchemesCatch p1 s =
    sendCatch $ (mkDescribeAnalysisSchemes p1) &~ s

-- $DescribeAvailabilityOptions
-- Gets the availability options configured for a domain. By default, shows
-- the configuration with any pending changes. Set the Deployed option to true
-- to show the active configuration and exclude pending changes. For more
-- information, see Configuring Availability Options in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.DescribeAvailabilityOptions'

describeAvailabilityOptions :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               )
    => Text -- ^ 'dao2DomainName'
    -> State DescribeAvailabilityOptions a
    -> m DescribeAvailabilityOptionsResponse
describeAvailabilityOptions p1 s =
    send $ (mkDescribeAvailabilityOptions p1) &~ s

describeAvailabilityOptionsCatch :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadReader Env m
                                    )
    => Text -- ^ 'dao2DomainName'
    -> State DescribeAvailabilityOptions a
    -> m (Either ServiceEr DescribeAvailabilityOptionsResponse)
describeAvailabilityOptionsCatch p1 s =
    sendCatch $ (mkDescribeAvailabilityOptions p1) &~ s

-- $DescribeDomains
-- Gets information about the search domains owned by this account. Can be
-- limited to specific domains. Shows all domains by default. To get the
-- number of searchable documents in a domain, use the console or submit a
-- matchall request to your domain's search endpoint:
-- q=matchall&amp;q.parser=structured&amp;size=0. For more information, see
-- Getting Information about a Search Domain in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.DescribeDomains'

describeDomains :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => State DescribeDomains a
    -> m DescribeDomainsResponse
describeDomains s =
    send (mkDescribeDomains &~ s)

describeDomainsCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => State DescribeDomains a
    -> m (Either ServiceEr DescribeDomainsResponse)
describeDomainsCatch s =
    sendCatch (mkDescribeDomains &~ s)

-- $DescribeExpressions
-- Gets the expressions configured for the search domain. Can be limited to
-- specific expressions by name. By default, shows all expressions and
-- includes any pending changes to the configuration. Set the Deployed option
-- to true to show the active configuration and exclude pending changes. For
-- more information, see Configuring Expressions in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.DescribeExpressions'

describeExpressions :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'de2DomainName'
    -> State DescribeExpressions a
    -> m DescribeExpressionsResponse
describeExpressions p1 s =
    send $ (mkDescribeExpressions p1) &~ s

describeExpressionsCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'de2DomainName'
    -> State DescribeExpressions a
    -> m (Either ServiceEr DescribeExpressionsResponse)
describeExpressionsCatch p1 s =
    sendCatch $ (mkDescribeExpressions p1) &~ s

-- $DescribeIndexFields
-- Gets information about the index fields configured for the search domain.
-- Can be limited to specific fields by name. By default, shows all fields and
-- includes any pending changes to the configuration. Set the Deployed option
-- to true to show the active configuration and exclude pending changes. For
-- more information, see Getting Domain Information in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.DescribeIndexFields'

describeIndexFields :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'dif2DomainName'
    -> State DescribeIndexFields a
    -> m DescribeIndexFieldsResponse
describeIndexFields p1 s =
    send $ (mkDescribeIndexFields p1) &~ s

describeIndexFieldsCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'dif2DomainName'
    -> State DescribeIndexFields a
    -> m (Either ServiceEr DescribeIndexFieldsResponse)
describeIndexFieldsCatch p1 s =
    sendCatch $ (mkDescribeIndexFields p1) &~ s

-- $DescribeScalingParameters
-- Gets the scaling parameters configured for a domain. A domain's scaling
-- parameters specify the desired search instance type and replication count.
-- For more information, see Configuring Scaling Options in the Amazon
-- CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.DescribeScalingParameters'

describeScalingParameters :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => Text -- ^ 'dspDomainName'
    -> m DescribeScalingParametersResponse
describeScalingParameters p1 =
    send (mkDescribeScalingParameters p1)

describeScalingParametersCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'dspDomainName'
    -> m (Either ServiceEr DescribeScalingParametersResponse)
describeScalingParametersCatch p1 =
    sendCatch (mkDescribeScalingParameters p1)

-- $DescribeServiceAccessPolicies
-- Gets information about the access policies that control access to the
-- domain's document and search endpoints. By default, shows the configuration
-- with any pending changes. Set the Deployed option to true to show the
-- active configuration and exclude pending changes. For more information, see
-- Configuring Access for a Search Domain in the Amazon CloudSearch Developer
-- Guide.
--
-- See: 'Network.AWS.CloudSearch.DescribeServiceAccessPolicies'

describeServiceAccessPolicies :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError AWS.Error m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'dsapDomainName'
    -> State DescribeServiceAccessPolicies a
    -> m DescribeServiceAccessPoliciesResponse
describeServiceAccessPolicies p1 s =
    send $ (mkDescribeServiceAccessPolicies p1) &~ s

describeServiceAccessPoliciesCatch :: ( MonadCatch m
                                      , MonadResource m
                                      , MonadReader Env m
                                      )
    => Text -- ^ 'dsapDomainName'
    -> State DescribeServiceAccessPolicies a
    -> m (Either ServiceEr DescribeServiceAccessPoliciesResponse)
describeServiceAccessPoliciesCatch p1 s =
    sendCatch $ (mkDescribeServiceAccessPolicies p1) &~ s

-- $DescribeSuggesters
-- Gets the suggesters configured for a domain. A suggester enables you to
-- display possible matches before users finish typing their queries. Can be
-- limited to specific suggesters by name. By default, shows all suggesters
-- and includes any pending changes to the configuration. Set the Deployed
-- option to true to show the active configuration and exclude pending
-- changes. For more information, see Getting Search Suggestions in the Amazon
-- CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.DescribeSuggesters'

describeSuggesters :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'ds3DomainName'
    -> State DescribeSuggesters a
    -> m DescribeSuggestersResponse
describeSuggesters p1 s =
    send $ (mkDescribeSuggesters p1) &~ s

describeSuggestersCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'ds3DomainName'
    -> State DescribeSuggesters a
    -> m (Either ServiceEr DescribeSuggestersResponse)
describeSuggestersCatch p1 s =
    sendCatch $ (mkDescribeSuggesters p1) &~ s

-- $IndexDocuments
-- Tells the search domain to start indexing its documents using the latest
-- indexing options. This operation must be invoked to activate options whose
-- OptionStatus is RequiresIndexDocuments.
--
-- See: 'Network.AWS.CloudSearch.IndexDocuments'

indexDocuments :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'idDomainName'
    -> m IndexDocumentsResponse
indexDocuments p1 =
    send (mkIndexDocuments p1)

indexDocumentsCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'idDomainName'
    -> m (Either ServiceEr IndexDocumentsResponse)
indexDocumentsCatch p1 =
    sendCatch (mkIndexDocuments p1)

-- $ListDomainNames
-- Lists all search domains owned by an account.
--
-- See: 'Network.AWS.CloudSearch.ListDomainNames'

listDomainNames :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => m ListDomainNamesResponse
listDomainNames =
    send (mkListDomainNames)

listDomainNamesCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => m (Either ServiceEr ListDomainNamesResponse)
listDomainNamesCatch =
    sendCatch (mkListDomainNames)

-- $UpdateAvailabilityOptions
-- Configures the availability options for a domain. Enabling the Multi-AZ
-- option expands an Amazon CloudSearch domain to an additional Availability
-- Zone in the same Region to increase fault tolerance in the event of a
-- service disruption. Changes to the Multi-AZ option can take about half an
-- hour to become active. For more information, see Configuring Availability
-- Options in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.UpdateAvailabilityOptions'

updateAvailabilityOptions :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => Text -- ^ 'uaoDomainName'
    -> Bool -- ^ 'uaoMultiAZ'
    -> m UpdateAvailabilityOptionsResponse
updateAvailabilityOptions p1 p2 =
    send (mkUpdateAvailabilityOptions p1 p2)

updateAvailabilityOptionsCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'uaoDomainName'
    -> Bool -- ^ 'uaoMultiAZ'
    -> m (Either ServiceEr UpdateAvailabilityOptionsResponse)
updateAvailabilityOptionsCatch p1 p2 =
    sendCatch (mkUpdateAvailabilityOptions p1 p2)

-- $UpdateScalingParameters
-- Configures scaling parameters for a domain. A domain's scaling parameters
-- specify the desired search instance type and replication count. Amazon
-- CloudSearch will still automatically scale your domain based on the volume
-- of data and traffic, but not below the desired instance type and
-- replication count. If the Multi-AZ option is enabled, these values control
-- the resources used per Availability Zone. For more information, see
-- Configuring Scaling Options in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.UpdateScalingParameters'

updateScalingParameters :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => Text -- ^ 'uspDomainName'
    -> ScalingParameters -- ^ 'uspScalingParameters'
    -> m UpdateScalingParametersResponse
updateScalingParameters p1 p2 =
    send (mkUpdateScalingParameters p1 p2)

updateScalingParametersCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => Text -- ^ 'uspDomainName'
    -> ScalingParameters -- ^ 'uspScalingParameters'
    -> m (Either ServiceEr UpdateScalingParametersResponse)
updateScalingParametersCatch p1 p2 =
    sendCatch (mkUpdateScalingParameters p1 p2)

-- $UpdateServiceAccessPolicies
-- Configures the access rules that control access to the domain's document
-- and search endpoints. For more information, see Configuring Access for an
-- Amazon CloudSearch Domain.
--
-- See: 'Network.AWS.CloudSearch.UpdateServiceAccessPolicies'

updateServiceAccessPolicies :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               )
    => Text -- ^ 'usapDomainName'
    -> Text -- ^ 'usapAccessPolicies'
    -> m UpdateServiceAccessPoliciesResponse
updateServiceAccessPolicies p1 p2 =
    send (mkUpdateServiceAccessPolicies p1 p2)

updateServiceAccessPoliciesCatch :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadReader Env m
                                    )
    => Text -- ^ 'usapDomainName'
    -> Text -- ^ 'usapAccessPolicies'
    -> m (Either ServiceEr UpdateServiceAccessPoliciesResponse)
updateServiceAccessPoliciesCatch p1 p2 =
    sendCatch (mkUpdateServiceAccessPolicies p1 p2)
